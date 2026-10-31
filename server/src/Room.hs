module Room where

import Control.Concurrent.STM
import qualified Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (replicateM, void, when)
import Data.Time (UTCTime, getCurrentTime)
import Data.Aeson (encode)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Network.WebSockets as WS
import System.Random (randomRIO)

import DB (SavedPlayer(..), addExp, saveMeta)
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
  , roomDbIds   :: TVar (Map Int Int)       -- id trong phòng -> id trong bảng players
  , roomStarted :: TVar (Maybe UTCTime)     -- lúc bắt đầu ván
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
               <*> newTVarIO Map.empty
               <*> newTVarIO Nothing
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
joinRoom :: Room -> String -> Maybe SavedPlayer -> IO (Either String Int)
joinRoom room name saved = atomically $ do
  gs <- readTVar (roomState room)
  case phase gs of
    Lobby
      | length (profiles gs) >= 4 -> pure (Left "Phong da day")
      | otherwise -> do
          pid <- readTVar (roomNextId room)
          writeTVar (roomNextId room) (pid + 1)
          let profile = case saved of
                Just sp -> PlayerProfile name (spRole sp) (spLevel sp) (spExp sp) (spGold sp) False (spMeta sp)
                Nothing -> PlayerProfile name Warrior 1 0 500 False defaultMeta
          case saved of
            Just sp -> modifyTVar' (roomDbIds room) (Map.insert pid (spId sp))
            Nothing -> pure ()
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

-- Bật / tắt sẵn sàng ở lobby.
setReady :: Room -> Int -> Bool -> IO ()
setReady room pid flag = atomically $ modifyTVar' (roomState room) $ \gs -> case phase gs of
  Lobby -> gs { profiles = [ (i, if i == pid then p { profileReady = flag } else p) | (i, p) <- profiles gs ] }
  _ -> gs

-- Chủ phòng (id nhỏ nhất) bắt đầu ván: Lobby -> Playing.
startGame :: Room -> Int -> IO (Either String ())
startGame room pid = getCurrentTime >>= \now -> atomically $ do
  gs <- readTVar (roomState room)
  case (phase gs, map fst (profiles gs)) of
    (Lobby, ids@(_ : _))
      | pid /= minimum ids -> pure (Left "Chi chu phong moi duoc bat dau")
      | not (all (profileReady . snd) [x | x@(i, _) <- profiles gs, i /= pid]) ->
          pure (Left "Con nguoi choi chua san sang")
      | otherwise -> do
          writeTVar (roomState room) gs { phase = Playing (newMapState (profiles gs)) }
          writeTVar (roomStarted room) (Just now)
          pure (Right ())
    (Lobby, []) -> pure (Left "Phong chua co nguoi choi")
    _ -> pure (Left "Van dau da bat dau")

-- Gửi một ServerMsg cho mọi người trong phòng (lỗi gửi bị bỏ qua).
broadcast :: Room -> ServerMsg -> IO ()
broadcast room msg = do
  conns <- readTVarIO (roomClients room)
  let bytes = encode msg
  mapM_ (\c -> void (try (WS.sendTextData c bytes) :: IO (Either SomeException ()))) (Map.elems conns)

-- Lấy (và xoá về 0) gold/exp chưa lưu của một người chơi đang trong ván.
-- Trả về (dbId, gold, exp) nếu người đó có tài khoản DB và có gì để lưu.
takePending :: Room -> Int -> IO (Maybe (Int, Int, Int))
takePending room pid = atomically $ do
  gs <- readTVar (roomState room)
  ids <- readTVar (roomDbIds room)
  case (phase gs, Map.lookup pid ids) of
    (Playing ms, Just dbId) ->
      case [p | p <- players ms, playerId p == pid] of
        (p : _) | pendingGold p /= 0 || pendingExp p /= 0 -> do
          let clear q = if playerId q == pid then q { pendingGold = 0, pendingExp = 0 } else q
              -- phần đã lưu được cộng vào hồ sơ trong phòng để HUD không bị tụt số
              bump (i, pr)
                | i == pid =
                    let (lv, ex) = addExp (profileLv pr) (profileExp pr) (pendingExp p)
                    in (i, pr { profileLv = lv, profileExp = ex, profileGold = profileGold pr + pendingGold p })
                | otherwise = (i, pr)
          writeTVar (roomState room) gs
            { phase = Playing ms { players = map clear (players ms) }
            , profiles = map bump (profiles gs) }
          pure (Just (dbId, pendingGold p, pendingExp p))
        _ -> pure Nothing
    _ -> pure Nothing

-- Cộng/trừ vàng (mở rương, mua đồ). Trừ chỉ được khi đủ vàng (đã lưu + vừa nhặt).
adjustGold :: Room -> Int -> Int -> IO ()
adjustGold room pid delta0 = atomically $ do
  gs <- readTVar (roomState room)
  case phase gs of
    Playing ms ->
      case ([p | p <- players ms, playerId p == pid], lookup pid (profiles gs)) of
        (p : _, Just pr) -> do
          let delta = max (-5000) (min 1000 delta0)
              available = profileGold pr + pendingGold p
              add q = if playerId q == pid then q { pendingGold = pendingGold q + delta } else q
          when (delta >= 0 || available + delta >= 0) $
            writeTVar (roomState room) gs { phase = Playing ms { players = map add (players ms) } }
        _ -> pure ()
    _ -> pure ()

-- Hồi máu cho người chơi còn sống (tối đa tới maxHp).
healPlayer :: Room -> Int -> Int -> IO ()
healPlayer room pid amount0 = atomically $ modifyTVar' (roomState room) $ \gs ->
  case phase gs of
    Playing ms -> gs { phase = Playing ms { players = map heal (players ms) } }
    _ -> gs
  where
    amount = max 0 (min 1000 amount0)
    heal p
      | playerId p == pid && status p == Alive =
          p { stats = (stats p) { hp = min (maxHp (stats p)) (hp (stats p) + amount) } }
      | otherwise = p

-- Tăng chỉ số cho người chơi (mua bùa / giày / nâng cấp ở shop). Có giới hạn để không bị lạm dụng.
-- Tăng máu tối đa thì máu hiện tại tăng theo cùng lượng.
boostPlayer :: Room -> Int -> Int -> Float -> Int -> IO ()
boostPlayer room pid dDmg0 dSpd0 dHp0 = atomically $ modifyTVar' (roomState room) $ \gs ->
  case phase gs of
    Playing ms -> gs { phase = Playing ms { players = map boost (players ms) } }
    _ -> gs
  where
    dDmg = max 0 (min 20 dDmg0)
    dSpd = max 0 (min 2 dSpd0)
    dHp  = max 0 (min 100 dHp0)
    boost p
      | playerId p == pid && status p == Alive =
          let st = stats p
              newMax = min 1000 (maxHp st + dHp)
          in p { stats = st
                   { damage = min 500 (damage st + dDmg)
                   , moveSpeed = min 20 (moveSpeed st + dSpd)
                   , maxHp = newMax
                   , hp = min newMax (hp st + (newMax - maxHp st))
                   } }
      | otherwise = p

-- Ván kết thúc xong (đã lưu DB) thì đưa phòng về Lobby để chơi tiếp / người mới vào được.
-- Vàng, EXP vừa kiếm được cộng vào hồ sơ trong phòng; người đã rời thì bỏ khỏi danh sách.
resetToLobby :: Room -> MapState -> IO ()
resetToLobby room ms = atomically $ do
  gs <- readTVar (roomState room)
  clients <- readTVar (roomClients room)
  ids <- readTVar (roomDbIds room)
  case phase gs of
    RoomResult _ -> do
      let earned pid =
            [ p | p <- players ms, playerId p == pid, status p /= Disconnected, Map.member pid ids ]
          bump (i, pr) = case earned i of
            (p : _) ->
              let (lv, ex) = addExp (profileLv pr) (profileExp pr) (pendingExp p)
              in (i, pr { profileLv = lv, profileExp = ex
                        , profileGold = max 0 (profileGold pr + pendingGold p), profileReady = False })
            [] -> (i, pr { profileReady = False })
      writeTVar (roomState room) gs
        { phase = Lobby
        , profiles = map bump (filter ((`Map.member` clients) . fst) (profiles gs)) }
      writeTVar (roomStarted room) Nothing
    _ -> pure ()

-- Chọn nghề ở lobby (quyết định chỉ số, tầm đánh và hình hiện trong phòng chờ).
setRole :: Room -> Int -> Role -> IO ()
setRole room pid role = atomically $ modifyTVar' (roomState room) $ \gs -> case phase gs of
  Lobby -> gs { profiles = [ (i, if i == pid then p { profileRole = role } else p) | (i, p) <- profiles gs ] }
  _ -> gs

-- Lưu gem / cấp nâng cấp: cập nhật hồ sơ trong phòng (để ván sau áp dụng) và ghi database.
-- Giá trị bị chặn trong khoảng hợp lý.
updateMeta :: Room -> Int -> Meta -> IO ()
updateMeta room pid m0 = do
  let clampTo hi v = max 0 (min hi v)
      m = Meta (clampTo 99999 (metaGems m0)) (clampTo 50 (metaHpLv m0))
               (clampTo 50 (metaDmgLv m0)) (clampTo 50 (metaSpeedLv m0))
               (clampTo 50 (metaArcherHpLv m0)) (clampTo 50 (metaArcherDmgLv m0))
               (clampTo 50 (metaArcherSpeedLv m0))
  atomically $ modifyTVar' (roomState room) $ \gs ->
    gs { profiles = [ (i, if i == pid then p { profileMeta = m } else p) | (i, p) <- profiles gs ] }
  ids <- readTVarIO (roomDbIds room)
  case Map.lookup pid ids of
    Just dbId -> void (forkIO' (saveMeta dbId m))
    Nothing -> pure ()
  where forkIO' = Control.Concurrent.forkIO
