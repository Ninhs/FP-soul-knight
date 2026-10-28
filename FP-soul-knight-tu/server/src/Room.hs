module Room where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random (randomRIO)
import Control.Monad (replicateM)

import Types.GameState

-- Toàn bộ các phòng đang mở, key = mã phòng
type Rooms = TVar (Map String (TVar GameState))

newRooms :: IO Rooms
newRooms = newTVarIO Map.empty

-- Sinh mã phòng 6 ký tự ngẫu nhiên
genRoomCode :: IO String
genRoomCode = replicateM 6 (randomRIO ('A', 'Z'))

-- Tạo phòng mới, trả về mã phòng
createRoom :: Rooms -> IO String
createRoom rooms = do
  code <- genRoomCode
  stateVar <- newTVarIO (GameState Lobby [])
  atomically $ modifyTVar' rooms (Map.insert code stateVar)
  pure code

-- Lấy TVar GameState của 1 phòng theo mã, Nothing nếu không tồn tại
lookupRoom :: Rooms -> String -> IO (Maybe (TVar GameState))
lookupRoom rooms code = Map.lookup code <$> readTVarIO rooms

-- Thêm người chơi vào phòng (chỉ khi đang ở Lobby và chưa đủ 4 người)
joinRoom :: TVar GameState -> Int -> PlayerProfile -> IO (Either String ())
joinRoom stateVar pid profile = atomically $ do
  gs <- readTVar stateVar
  case phase gs of
    Lobby
      | length (profiles gs) >= 4 -> pure (Left "Phong da day")
      | otherwise -> do
          writeTVar stateVar gs { profiles = profiles gs ++ [(pid, profile)] }
          pure (Right ())
    _ -> pure (Left "Phong da bat dau, khong the vao")