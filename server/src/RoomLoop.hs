-- Vòng lặp game của MỘT phòng: đọc input -> cập nhật -> phát StateUpdate.
module RoomLoop (roomLoop) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.Map as M
import GHC.Clock (getMonotonicTime)

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
      pure cds'
