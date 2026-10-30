{-# LANGUAGE OverloadedStrings #-}
module Network where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Exception (finally, try, SomeException)
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (encode, decode)
import Data.Char (toUpper)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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
app rooms pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    result <- try (handleClient rooms conn) :: IO (Either SomeException ())
    case result of
      Left e  -> putStrLn ("Client loi/mat ket noi: " ++ show e)
      Right _ -> pure ()

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
          joinResult <- joinRoom room name
          case joinResult of
            Left err  -> sendMsg (RoomError err)
            Right pid -> do
              gs <- readTVarIO (roomState room)
              sendMsg (RoomJoined pid code gs)
              registerClient room pid conn
              clientLoop conn room pid `finally` leaveRoom rooms code room pid
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
        Just LeaveRoom -> pure ()
        _ -> loop
