module Logic.GameLoop
  ( gameLoop
  , updateStep
  ) where

import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.List (partition)

import Logic.Movement (distance)
import Logic.Combat
  ( stepBullets
  , resolvePlayerBullets
  , resolveMonsterBullets
  , splitDeadMonsters
  , circlesOverlap
  , playerRadius
  )
import Logic.AI (updateAI)
import Logic.Portal (updatePortal, roomOutcome)
import Types.GameState

-- Hằng số vòng lặp
targetFPS :: Int
targetFPS = 60

frameTimeMicros :: Int
frameTimeMicros = 1000000 `div` targetFPS  -- ~16666 microseconds (16.6ms)

dt :: Float
dt = 1.0 / fromIntegral targetFPS          -- ~0.01667 giây mỗi frame

-- Bán kính nhặt đồ
pickupRadius :: Float
pickupRadius = 1.2

-- 1. VÒNG LẶP CHÍNH (Game Loop Run)

-- Vòng lặp vô hạn chạy trên Server thread
gameLoop :: TVar GameState -> IO ()
gameLoop tvState = forever $ do
  gs <- readTVarIO tvState
  case phase gs of
    Playing ms -> do
      -- A. Lấy danh sách quái đã chết ở frame này
      let (_, deadMonsters) = splitDeadMonsters (monsters ms)
      
      -- B. Sinh Drop ngẫu nhiên trong IO cho từng con quái chết
      newRandomDrops <- mapM generateDropsIO deadMonsters
      let dropsToAdd = concat newRandomDrops

      -- C. Cập nhật MapState (truyền dropsToAdd vào updateStep)
      let ms' = updateStep dt dropsToAdd ms
      
      let nextPhase = case roomOutcome ms' of
                        Just result -> RoomResult result
                        Nothing     -> Playing ms'
      
      atomically $ writeTVar tvState (gs { phase = nextPhase })

    _ -> pure () -- Nếu ở Lobby hoặc RoomResult thì không update

  threadDelay frameTimeMicros

-- 2. TÍNH TOÁN THEO MỖI FRAME (Tick Step)

-- Tham số `newDrops` được nhận từ IO truyền vào
updateStep :: Float -> [Drop] -> MapState -> MapState
updateStep deltaTime newDrops ms0 = 
  let
    -- 1. Di chuyển đạn & xóa đạn hết hạn / chạm tường
    bulletsMoved = stepBullets (\_ -> False) deltaTime (bullets ms0)

    -- 2. Xử lý va chạm đạn của Người chơi -> Quái
    (bulletsAfterP, monstersDamaged) = resolvePlayerBullets bulletsMoved (monsters ms0)

    -- 3. Xử lý va chạm đạn của Quái -> Người chơi
    (bulletsFinal, playersDamaged) = resolveMonsterBullets bulletsAfterP (players ms0)

    -- 4. Tách quái đã chết & nạp Drop mới vừa sinh ngẫu nhiên vào map
    (aliveMonsters, _) = splitDeadMonsters monstersDamaged
    allDrops = drops ms0 ++ newDrops

    -- 5. Xử lý Người chơi nhặt Drop (Vàng, Exp, Máu)
    (remainingDrops, playersAfterDrops) = collectDrops allDrops playersDamaged

    -- 6. Gom dữ liệu trung gian vào MapState
    ms1 = ms0
      { players   = playersAfterDrops
      , monsters  = aliveMonsters
      , bullets   = bulletsFinal
      , drops     = remainingDrops
      }

    -- 7. Cập nhật AI Quái vật (đuổi theo & bắn người chơi)
    ms2 = updateAI deltaTime ms1

    -- 8. Cập nhật trạng thái Cổng (mở cổng khi diệt sạch Boss)
    ms3 = updatePortal ms2

  in ms3

-- 3. LOGIC RỚT ĐỒ & NHẶT ĐỒ (Drop System)

-- Quay số ngẫu nhiên từ 0 đến 99 trong IO cho 1 quái vật
generateDropsIO :: Monster -> IO [Drop]
generateDropsIO m = do
  roll <- randomRIO (0, 99 :: Int)
  pure (generateDropsWithRoll roll m)

-- Tính danh sách Drop theo tỷ lệ % dựa trên con số `roll`
generateDropsWithRoll :: Int -> Monster -> [Drop]
generateDropsWithRoll roll m = 
  let pos = mPosition m
  in case kind m of
        Boss -> 
          -- Boss: Luôn rớt Vàng 100 + EXP 100; Máu 100 rớt 70% (roll < 70)
          let heal = if roll < 70 then [Drop pos (HealDrop 100)] else []
          in heal ++ [Drop pos (GoldDrop 100), Drop pos (ExpDrop 100)]

        Regular _ _ -> 
          -- Quái thường:
          --  0 .. 49 (50%): Rớt Vàng (+20)
          -- 50 .. 79 (30%): Rớt EXP (+10)
          -- 80 .. 89 (10%): Rớt Máu (+20)
          -- 90 .. 99 (10%): Không rớt đồ ([])
          if roll < 50 then
            [Drop pos (GoldDrop 20)]
          else if roll < 80 then
            [Drop pos (ExpDrop 10)]
          else if roll < 90 then
            [Drop pos (HealDrop 20)]
          else
            []

-- Người chơi nhặt đồ gần
collectDrops :: [Drop] -> [Player] -> ([Drop], [Player])
collectDrops currentDrops ps = foldl processDrop ([], ps) currentDrops
  where
    processDrop (accDrops, currentPs) d =
      case findPicker currentPs d of
        Just pickerId -> 
          (accDrops, map (applyDrop pickerId (dType d)) currentPs)
        Nothing -> 
          (d : accDrops, currentPs)

    findPicker psList d = case filter (\p -> status p == Alive && canPick p d) psList of
      (p:_) -> Just (playerId p)
      []    -> Nothing

    canPick p d = distance (position p) (dPosition d) <= (playerRadius + pickupRadius)

    applyDrop pId dropType p
      | playerId p /= pId = p
      | otherwise = case dropType of
          GoldDrop g -> p { pendingGold = pendingGold p + g }
          ExpDrop e  -> p { pendingExp  = pendingExp p + e }
          HealDrop h -> 
            let st = stats p
                newHp = min (maxHp st) (hp st + h)
            in p { stats = st { hp = newHp } }