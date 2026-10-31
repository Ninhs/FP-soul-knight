module Main where

import Control.Concurrent.MVar (tryReadMVar)
import Control.Exception (SomeException, try)
import Data.Char (toUpper)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.IO.Game
  ( Event(..)
  , Key(..)
  , KeyState(..)
  , MouseButton(..)
  , SpecialKey(..)
  , playIO
  )
import Animation (AnimKind(..), setAnimKind, startAnim, stepAnim)
import Assets (loadAssets)
import ClientUI (UiAction(..), UiResult(..), reduceUi, startLocalRun, toEnemyData)
import Components.UI (insideRect)
import qualified MapLayout as Map
import Network
  ( NetworkConnection(..)
  , connectToServer
  )
import Protocol (ClientMsg(StartGame, InputMsg, LeaveRoom))
import qualified Types.GameState as GS
import ClientGameplay (stepLocalGameplay)
import Input
  ( InputRefs
  , handleEvent
  , movementDirection
  , newInputRefs
  , takeAttackRequest
  )
import Render (renderClient)
import Types

-- Kết nối mạng hiện tại (Nothing = đang chơi offline) và địa chỉ server.
data Net = Net
  { netConn :: IORef (Maybe NetworkConnection)
  , netHost :: String
  , netPort :: Int
  }

main :: IO ()
main = runClient

-- Địa chỉ server: đọc dòng đầu của file server.txt (dạng host:port),
-- nếu không có thì dùng biến môi trường SK_SERVER, cuối cùng là 127.0.0.1:8080.
loadServerAddress :: IO (String, Int)
loadServerAddress = do
  fileResult <- try (readFile "server.txt") :: IO (Either SomeException String)
  envResult <- lookupEnv "SK_SERVER"
  let fromFile = case fileResult of
        Right txt | (l : _) <- lines txt, not (null (trim l)) -> Just (trim l)
        _ -> Nothing
      raw = fromMaybe "127.0.0.1:8080" (maybe envResult Just fromFile)
      (host, rest) = break (== ':') raw
      port = fromMaybe 8080 (readMaybe (drop 1 rest))
  pure (host, port)
  where
    trim = filter (`notElem` " \r\n\t")

runClient :: IO ()
runClient = do
  assets <- loadAssets
  inputRefs <- newInputRefs
  (host, port) <- loadServerAddress
  connRef <- newIORef Nothing
  let net = Net connRef host port
      window = InWindow "Soul Knight Haskell" (1280, 720) (100, 100)
      initialState = initialClientState assets
  playIO window black 60 initialState
    (pure . renderClient)
    (handleInput net inputRefs)
    (updateFrame net inputRefs)

handleInput :: Net -> InputRefs -> Event -> ClientState -> IO ClientState
handleInput net inputRefs event state = do
  handleEvent (const (pure ())) inputRefs event
  let positionedState = case event of
        EventMotion position -> state { mousePos = position }
        EventKey (MouseButton _) _ _ position -> state { mousePos = position }
        _ -> state
  case event of
    EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      applyUiAction inputRefs UiEnter positionedState
    EventKey (Char key) Down _ _ ->
      applyUiAction inputRefs (UiKey key) positionedState
    EventKey (MouseButton LeftButton) Down _ (x, y)
      | appView state == LobbyView -> do
          handled <- lobbyClick net (x, y) positionedState
          case handled of
            Just nextState -> pure nextState
            Nothing -> applyUiAction inputRefs (UiClick (x, y)) positionedState
      | otherwise -> applyUiAction inputRefs (UiClick (x, y)) positionedState
    _ -> pure positionedState

-- Các nút ở lobby cần gọi mạng. Trả Nothing nếu để UI offline cũ xử lý.
lobbyClick :: Net -> (Float, Float) -> ClientState -> IO (Maybe ClientState)
lobbyClick net position state
  -- JOIN: nối server; ô mã phòng trống = tạo phòng mới
  | insideRect position (215, 215) 130 58 = do
      current <- readIORef (netConn net)
      case current of
        Just _ -> pure (Just state { statusText = "Already connected to a room." })
        Nothing -> do
          let code = map toUpper (roomInput state)
          result <- connectToServer (netHost net) (netPort net) code "Player"
          case result of
            Left err -> pure (Just state { statusText = "Connect failed: " ++ err })
            Right conn -> do
              writeIORef (netConn net) (Just conn)
              pure (Just state
                { roomCode = joinedRoomCode conn
                , roomInput = ""
                , roomInputActive = False
                , isMultiplayer = True
                , onlinePlayerId = Just (myPlayerId conn)
                , sharedState = initialGameState conn
                , statusText = "Joined room " ++ joinedRoomCode conn })
  -- START RUN: khi đã online thì chỉ chủ phòng được bắt đầu
  | insideRect position (150, -270) 220 52 = do
      current <- readIORef (netConn net)
      case current of
        Nothing -> pure Nothing
        Just conn -> do
          let ids = map fst (GS.profiles (sharedState state))
              isHost = ids /= [] && myPlayerId conn == minimum ids
          if isHost
            then do
              sendClientMessage conn StartGame
              pure (Just state { statusText = "Starting..." })
            else pure (Just state { statusText = "Only the host can start the run." })
  -- BACK: rời phòng rồi để UI cũ quay lại màn trước
  | insideRect position (-450, -270) 200 52 = do
      leaveRoom net
      pure Nothing
  | otherwise = pure Nothing

leaveRoom :: Net -> IO ()
leaveRoom net = do
  current <- readIORef (netConn net)
  case current of
    Just conn -> sendClientMessage conn LeaveRoom
    Nothing -> pure ()
  writeIORef (netConn net) Nothing

applyUiAction :: InputRefs -> UiAction -> ClientState -> IO ClientState
applyUiAction inputRefs action state =
  case reduceUi action state of
    UiQuit -> exitSuccess
    UiHandled nextState -> do
      case action of
        UiClick _ -> do
          _ <- takeAttackRequest inputRefs
          pure ()
        _ -> pure ()
      pure nextState
    UiUnhandled -> pure state

updateFrame :: Net -> InputRefs -> Float -> ClientState -> IO ClientState
updateFrame net inputRefs deltaTime state = do
  current <- readIORef (netConn net)
  case current of
    Nothing -> updateLocal inputRefs deltaTime state
    Just conn -> updateOnline net conn inputRefs deltaTime state

updateLocal :: InputRefs -> Float -> ClientState -> IO ClientState
updateLocal inputRefs deltaTime state
  | appView state /= InGameView = do
      _ <- takeAttackRequest inputRefs
      pure state
  | otherwise = do
      direction <- movementDirection inputRefs
      attackPressed <- takeAttackRequest inputRefs
      pure (stepLocalGameplay deltaTime direction attackPressed state)

-- Chơi online: gửi input lên server, nhận GameState mới nhất và đổ vào
-- ClientState để Render cũ vẽ lại (không tự mô phỏng gameplay ở client).
updateOnline :: Net -> NetworkConnection -> InputRefs -> Float -> ClientState -> IO ClientState
updateOnline net conn inputRefs dt state = do
  failure <- tryReadMVar (connectionFailure conn)
  case failure of
    Just err -> do
      writeIORef (netConn net) Nothing
      pure state
        { appView = LobbyView
        , isMultiplayer = False
        , onlinePlayerId = Nothing
        , statusText = "Disconnected: " ++ err
        }
    Nothing -> do
      latest <- tryReadMVar (latestGameState conn)
      let gs = fromMaybe (sharedState state) latest
      direction <- movementDirection inputRefs
      _ <- takeAttackRequest inputRefs
      case GS.phase gs of
        GS.Lobby ->
          pure state
            { sharedState = gs
            , statusText = "Players in room: " ++ show (length (GS.profiles gs)) ++ "/4"
            }
        GS.Playing ms -> do
          -- tự động đánh (server chọn mục tiêu gần nhất trong tầm)
          let move = if appView state == InGameView then direction else (0, 0)
          sendClientMessage conn (InputMsg move (0, 0) True)
          let base = if appView state == LobbyView then freshOnlineRun state else state
          pure (applyServerMap dt direction gs ms base)
        GS.RoomResult result ->
          pure state
            { sharedState = gs
            , appView = if appView state == InGameView
                then (if result == GS.Victory then VictoryView else DefeatView)
                else appView state
            }

-- Khởi tạo các trường phụ (HUD, animation...) bằng startLocalRun rồi xoá quái giả.
freshOnlineRun :: ClientState -> ClientState
freshOnlineRun state =
  (startLocalRun state)
    { enemies = []
    , enemyCorpses = []
    , isMultiplayer = True
    , onlinePlayerId = onlinePlayerId state
    }

-- Đổ MapState từ server vào ClientState.
applyServerMap :: Float -> (Float, Float) -> GS.GameState -> GS.MapState -> ClientState -> ClientState
applyServerMap dt (dx, _) gs ms st = st
  { sharedState = gs
  , localMapState = ms
  , playerPosition = myScreen
  , playerPos = myScreen
  , enemies = newEnemies
  , enemyCorpses = map (\e -> e { enemyAnim = stepAnim dt (enemyAnim e) }) (enemyCorpses st ++ newCorpses)
  , clearedRooms = cleared
  , currentRoom = Map.roomAt (selectedMapSize st) myScreen
  , bossDefeated = not (any ((== 900) . GS.monsterId) (GS.monsters ms))
  , playerHealth = maybe (playerHealth st) (fromIntegral . GS.hp . GS.stats) me
  , playerMaxHealth = maybe (playerMaxHealth st) (fromIntegral . max 1 . GS.maxHp . GS.stats) me
  , playerAnim = stepAnim dt (setAnimKind (if moved then Walk else Idle) (playerAnim st))
  , playerFacingRight = if dx > 0 then True else if dx < 0 then False else playerFacingRight st
  }
  where
    me = find ((== onlinePlayerId st) . Just . GS.playerId) (GS.players ms)
    myScreen = maybe (playerPosition st) (Map.toScreenPosition . GS.position) me
    moved = distance2 myScreen (playerPosition st) > 0.01
    distance2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)
    cleared =
      [ r
      | r <- [NormalA, NormalB, BossRoom]
      , not (any ((== Just r) . Map.homeRoomOfMonster . GS.monsterId) (GS.monsters ms))
      ]
    oldById = [(enemyId e, e) | e <- enemies st]
    newIds = map GS.monsterId (GS.monsters ms)
    newEnemies = map carry (map toEnemyData (GS.monsters ms))
    carry e = case lookup (enemyId e) oldById of
      Nothing -> e
      Just old ->
        let (ox, _) = enemyPos old
            (nx, _) = enemyPos e
            movedE = distance2 (enemyPos old) (enemyPos e) > 0.01
        in e { enemyAnim = stepAnim dt (setAnimKind (if movedE then Walk else Idle) (enemyAnim old))
             , enemyFacingRight = if nx > ox then True else if nx < ox then False else enemyFacingRight old }
    -- quái có ở frame trước nhưng không còn trong danh sách sống = vừa chết
    newCorpses =
      [ old { enemyIsAlive = False, enemyAnim = startAnim Die }
      | old <- enemies st
      , enemyId old `notElem` newIds
      ]
