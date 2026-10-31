-- Vòng lặp game của MỘT phòng: đọc input -> cập nhật -> phát StateUpdate.
module RoomLoop (roomLoop) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Data.Time (getCurrentTime)
import qualified Data.Map as M
import GHC.Clock (getMonotonicTime)

import DB (RunPlayer(..), saveProgress, saveRun)
import Logic.Control (Cooldowns, applyInputs)
import Logic.GameLoop (generateDropsIO, updateStep)
import Logic.Portal (roomOutcome)
import Protocol
import Room
import Types.GameState

frameMicros :: Int
frameMicros = 1000000 `div` 60

-- Gửi StateUpdate mỗi `broadcastEvery` tick (60/2 = 30 lần/giây).
broadcastEvery :: Int
broadcastEvery = 2

autosaveEvery :: Int
autosaveEvery = 300

autosave :: Room -> IO ()
autosave room = do
  ids <- readTVarIO (roomDbIds room)
  mapM_ (\pid -> do
           pending <- takePending room pid
           case pending of
             Just (dbId, gold, ex) -> void (forkIO (saveProgress dbId gold ex))
             Nothing -> pure ())
        (M.keys ids)

-- Chạy cho tới khi phòng bị xoá khỏi `rooms` (hết người).
roomLoop :: Rooms -> String -> Room -> IO ()
roomLoop rooms code room = getMonotonicTime >>= go (0 :: Int) M.empty
  where
    go n cds tPrev = do
      present <- M.member code <$> readTVarIO rooms
      when present $ do
        now <- getMonotonicTime
        let dt = realToFrac (max 0.001 (min 0.05 (now - tPrev))) :: Float
        cds' <- tick dt cds
        -- tự lưu vàng/EXP nhặt được mỗi ~5 giây (phòng khi người chơi mất kết nối đột ngột)
        when (n > 0 && n `mod` autosaveEvery == 0) (autosave room)
        when (n `mod` broadcastEvery == 0) $
          readTVarIO (roomState room) >>= broadcast room . StateUpdate
        threadDelay frameMicros
        go (n + 1) cds' now

    -- Toàn bộ bước cập nhật chạy trong MỘT giao dịch STM trên trạng thái mới nhất,
    -- để không ghi đè thay đổi của thread khác (nhặt rương, mua đồ, tự lưu...).
    tick :: Float -> Cooldowns -> IO Cooldowns
    tick dt cds = do
      inputs <- readTVarIO (roomInputs room)
      (cds', killed, finished) <- atomically $ do
        g <- readTVar (roomState room)
        case phase g of
          Playing ms0 -> do
            let (ms1, cdsNext) = applyInputs dt inputs cds ms0
                ms2 = updateStep dt [] ms1
                aliveIds = map monsterId (monsters ms2)
                dead = [m | m <- monsters ms1, monsterId m `notElem` aliveIds]
                next = maybe (Playing ms2) RoomResult (roomOutcome ms2)
            writeTVar (roomState room) g { phase = next }
            pure (cdsNext, dead, case next of
                                   RoomResult r -> Just (r, ms2)
                                   _            -> Nothing)
          _ -> pure (cds, [], Nothing)
      -- quái chết rớt đồ (cần random nên làm ngoài STM)
      newDrops <- concat <$> mapM generateDropsIO killed
      when (not (null newDrops)) $
        atomically $ modifyTVar' (roomState room) $ \g -> case phase g of
          Playing ms -> g { phase = Playing ms { drops = drops ms ++ newDrops } }
          _          -> g
      -- Ván vừa kết thúc (chỉ xảy ra đúng 1 lần): lưu kết quả vào DB ở thread riêng
      case finished of
        Just (r, ms) -> void (forkIO (persistRun code room r ms))
        Nothing      -> pure ()
      pure cds'

-- Gom kết quả từng người chơi (đã có tài khoản DB) rồi lưu một lượt.
persistRun :: String -> Room -> Result -> MapState -> IO ()
persistRun code room result ms = do
  dbIds   <- readTVarIO (roomDbIds room)
  started <- readTVarIO (roomStarted room)
  gs      <- readTVarIO (roomState room)
  now     <- getCurrentTime
  let hostPid = case map fst (profiles gs) of
        [] -> -1
        ids -> minimum ids
      roleOf pid = maybe Warrior profileRole (lookup pid (profiles gs))
      rps = [ RunPlayer dbId (roleOf (playerId p)) (playerId p == hostPid)
                        (status p) (pendingGold p) (pendingExp p)
            | p <- players ms
            , status p /= Disconnected
            , Just dbId <- [M.lookup (playerId p) dbIds] ]
  when (not (null rps)) $
    saveRun code (maybe now id started) result (mapIndex ms) rps
