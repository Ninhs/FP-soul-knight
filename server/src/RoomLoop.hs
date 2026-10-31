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
        gs <- readTVarIO (roomState room)
        cds' <- case phase gs of
          Playing ms -> tick dt cds ms
          _          -> pure cds
        -- tự lưu vàng/EXP nhặt được mỗi ~5 giây (phòng khi người chơi mất kết nối đột ngột)
        when (n > 0 && n `mod` autosaveEvery == 0) (autosave room)
        when (n `mod` broadcastEvery == 0) $
          readTVarIO (roomState room) >>= broadcast room . StateUpdate
        threadDelay frameMicros
        go (n + 1) cds' now

    tick :: Float -> Cooldowns -> MapState -> IO Cooldowns
    tick dt cds ms0 = do
      inputs <- readTVarIO (roomInputs room)
      let (ms1, cds') = applyInputs dt inputs cds ms0
          ms2 = updateStep dt [] ms1
          aliveIds = map monsterId (monsters ms2)
          killed = [m | m <- monsters ms1, monsterId m `notElem` aliveIds]
      newDrops <- concat <$> mapM generateDropsIO killed
      let ms3 = ms2 { drops = drops ms2 ++ newDrops }
          next = maybe (Playing ms3) RoomResult (roomOutcome ms3)
      -- chỉ ghi phase; giữ nguyên profiles do thread khác có thể vừa sửa
      atomically $ modifyTVar' (roomState room) $ \g -> case phase g of
        Playing _ -> g { phase = next }
        _         -> g
      -- Ván vừa kết thúc (chỉ xảy ra đúng 1 lần): lưu kết quả vào DB ở thread riêng
      case next of
        RoomResult r -> void (forkIO (persistRun code room r ms3))
        _            -> pure ()
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
