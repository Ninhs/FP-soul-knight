{-# LANGUAGE OverloadedStrings #-}
module Network where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Exception (finally, try, SomeException)
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (encode, decode)
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import DB (loadPlayer, saveProgress)
import Logic.Control (PlayerInput(..))
import Protocol
import Room
import RoomLoop (roomLoop)

runServer :: Rooms -> IO ()
runServer rooms = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8080 id (portStr >>= readMaybe)
  putStrLn ("Server dang chay tai port " ++ show port)
  WS.runServer "0.0.0.0" port (app rooms)

app :: Rooms -> WS.ServerApp
app rooms pending
  | isWebSocketUpgrade pending = do
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $ do
        result <- try (handleClient rooms conn) :: IO (Either SomeException ())
        case result of
          Left e  -> putStrLn ("Client loi/mat ket noi: " ++ show e)
          Right _ -> pure ()
  -- Yêu cầu HTTP thường (UptimeRobot, trình duyệt): trả 200 để Render coi là còn sống
  | otherwise =
      WS.rejectRequestWith pending WS.defaultRejectRequest
        { WS.rejectCode = 200
        , WS.rejectMessage = "OK"
        , WS.rejectBody = "FP Soul Knight server is running"
        }

-- Có header "Upgrade: websocket" hay không (so sánh không phân biệt hoa thường).
isWebSocketUpgrade :: WS.PendingConnection -> Bool
isWebSocketUpgrade pending =
  any (\(k, v) -> low k == "\"upgrade\"" && "websocket" `isInfixOf` low v)
      (WS.requestHeaders (WS.pendingRequest pending))
  where
    low :: Show a => a -> String
    low = map toLower . show

-- Tin nhắn đầu tiên phải là JoinRoom. Mã phòng rỗng = tạo phòng mới.
handleClient :: Rooms -> WS.Connection -> IO ()
handleClient rooms conn = do
  raw <- WS.receiveData conn
  case decode raw :: Maybe ClientMsg of
    Just (JoinRoom code0 name) -> do
      target <- if null code0
        then do
          (c, r) <- createRoom rooms
          _ <- forkIO (roomLoop rooms c r)
          putStrLn ("Phong moi: " ++ c)
          pure (Just (c, r))
        else do
          let c = map toUpper code0
          fmap (\r -> (c, r)) <$> lookupRoom rooms c
      case target of
        Nothing -> sendMsg (RoomError "Ma phong khong ton tai")
        Just (code, room) -> do
          saved <- loadPlayer name
          joinResult <- joinRoom room name saved
          case joinResult of
            Left err  -> sendMsg (RoomError err)
            Right pid -> do
              gs <- readTVarIO (roomState room)
              sendMsg (RoomJoined pid code gs)
              registerClient room pid conn
              clientLoop conn room pid `finally` (saveLeaver room pid >> leaveRoom rooms code room pid)
    _ -> sendMsg (RoomError "Message dau tien phai la JoinRoom")
  where
    sendMsg = WS.sendTextData conn . encode

-- Nhận tin nhắn của một người chơi cho tới khi họ rời phòng / mất kết nối.
clientLoop :: WS.Connection -> Room -> Int -> IO ()
clientLoop conn room pid = loop
  where
    loop = do
      msgRaw <- WS.receiveData conn
      case decode msgRaw :: Maybe ClientMsg of
        Just (InputMsg mv _ atk) -> setInput room pid (PlayerInput mv atk) >> loop
        Just StartGame -> do
          r <- startGame room pid
          case r of
            Left err -> WS.sendTextData conn (encode (RoomError err))
            Right () -> pure ()
          loop
        Just (AdjustGold d) -> adjustGold room pid d >> loop
        Just (HealSelf n) -> healPlayer room pid n >> loop
        Just (SaveMeta m) -> updateMeta room pid m >> loop
        Just (ChooseRole r) -> setRole room pid r >> loop
        Just (SetReady b) -> setReady room pid b >> loop
        Just (BoostStats d sp h) -> boostPlayer room pid d sp h >> loop
        Just LeaveRoom -> pure ()
        _ -> loop

-- Người chơi thoát giữa ván: lưu ngay gold/exp đã nhặt.
saveLeaver :: Room -> Int -> IO ()
saveLeaver room pid = do
  pending <- takePending room pid
  case pending of
    Just (dbId, gold, ex) -> saveProgress dbId gold ex
    Nothing -> pure ()
