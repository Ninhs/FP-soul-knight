module Room where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (replicateM, void)
import Data.Aeson (encode)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Network.WebSockets as WS
import System.Random (randomRIO)

import Logic.Control (PlayerInput)
import Logic.MapGen (newMapState)
import Protocol
import Types.GameState

-- Một phòng chơi: trạng thái game + input + các kết nối đang mở.
data Room = Room
  { roomState   :: TVar GameState
  , roomInputs  :: TVar (Map Int PlayerInput)
  , roomClients :: TVar (Map Int WS.Connection)
  , roomNextId  :: TVar Int
  }

-- Toàn bộ các phòng đang mở, key = mã phòng
type Rooms = TVar (Map String Room)

newRooms :: IO Rooms
newRooms = newTVarIO Map.empty

-- Sinh mã phòng 6 ký tự ngẫu nhiên
genRoomCode :: IO String
genRoomCode = replicateM 6 (randomRIO ('A', 'Z'))

-- Tạo phòng mới (mã không trùng), trả về mã phòng và phòng
createRoom :: Rooms -> IO (String, Room)
createRoom rooms = do
  code <- genRoomCode
  room <- Room <$> newTVarIO initialGameState
               <*> newTVarIO Map.empty
               <*> newTVarIO Map.empty
               <*> newTVarIO 1
  ok <- atomically $ do
    m <- readTVar rooms
    if Map.member code m
      then pure False
      else writeTVar rooms (Map.insert code room m) >> pure True
  if ok then pure (code, room) else createRoom rooms

-- Lấy phòng theo mã, Nothing nếu không tồn tại
lookupRoom :: Rooms -> String -> IO (Maybe Room)
lookupRoom rooms code = Map.lookup code <$> readTVarIO rooms

-- Thêm người chơi vào phòng (chỉ khi đang ở Lobby và chưa đủ 4 người).
-- Cấp id mới tăng dần, trả về id đó.
joinRoom :: Room -> String -> IO (Either String Int)
joinRoom room name = atomically $ do
  gs <- readTVar (roomState room)
  case phase gs of
    Lobby
      | length (profiles gs) >= 4 -> pure (Left "Phong da day")
      | otherwise -> do
          pid <- readTVar (roomNextId room)
          writeTVar (roomNextId room) (pid + 1)
          let profile = PlayerProfile name Warrior 1 0 0
          writeTVar (roomState room) gs { profiles = profiles gs ++ [(pid, profile)] }
          pure (Right pid)
    _ -> pure (Left "Phong da bat dau, khong the vao")

-- Đăng ký kết nối để nhận StateUpdate (gọi SAU khi đã gửi RoomJoined).
registerClient :: Room -> Int -> WS.Connection -> IO ()
registerClient room pid conn = atomically $ modifyTVar' (roomClients room) (Map.insert pid conn)

-- Người chơi rời phòng / mất kết nối. Phòng trống thì xoá (vòng lặp phòng tự dừng).
leaveRoom :: Rooms -> String -> Room -> Int -> IO ()
leaveRoom rooms code room pid = atomically $ do
  modifyTVar' (roomClients room) (Map.delete pid)
  modifyTVar' (roomInputs room) (Map.delete pid)
  modifyTVar' (roomState room) $ \gs -> case phase gs of
    Lobby -> gs { profiles = filter ((/= pid) . fst) (profiles gs) }
    Playing ms -> gs { phase = Playing ms { players = map (markGone pid) (players ms) } }
    _ -> gs
  remaining <- readTVar (roomClients room)
  if Map.null remaining then modifyTVar' rooms (Map.delete code) else pure ()
  where
    markGone i p = if playerId p == i then p { status = Disconnected } else p

-- Ghi input mới nhất của một người chơi.
setInput :: Room -> Int -> PlayerInput -> IO ()
setInput room pid inp = atomically $ modifyTVar' (roomInputs room) (Map.insert pid inp)

-- Chủ phòng (id nhỏ nhất) bắt đầu ván: Lobby -> Playing.
startGame :: Room -> Int -> IO (Either String ())
startGame room pid = atomically $ do
  gs <- readTVar (roomState room)
  case (phase gs, map fst (profiles gs)) of
    (Lobby, ids@(_ : _))
      | pid /= minimum ids -> pure (Left "Chi chu phong moi duoc bat dau")
      | otherwise -> do
          writeTVar (roomState room) gs { phase = Playing (newMapState (profiles gs)) }
          pure (Right ())
    (Lobby, []) -> pure (Left "Phong chua co nguoi choi")
    _ -> pure (Left "Van dau da bat dau")

-- Gửi một ServerMsg cho mọi người trong phòng (lỗi gửi bị bỏ qua).
broadcast :: Room -> ServerMsg -> IO ()
broadcast room msg = do
  conns <- readTVarIO (roomClients room)
  let bytes = encode msg
  mapM_ (\c -> void (try (WS.sendTextData c bytes) :: IO (Either SomeException ()))) (Map.elems conns)
