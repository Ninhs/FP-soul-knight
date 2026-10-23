{-# LANGUAGE OverloadedStrings #-}
module Network where

import qualified Network.WebSockets as WS
import Control.Exception (finally, try, SomeException)
import Control.Monad (forever)
import Data.Aeson (encode, decode)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Control.Concurrent.STM (readTVarIO)

import Protocol
import Room
import Types.GameState

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

handleClient :: Rooms -> WS.Connection -> IO ()
handleClient rooms conn = do
  raw <- WS.receiveData conn
  case decode raw :: Maybe ClientMsg of
    Just (JoinRoom code name) -> do
      mRoom <- lookupRoom rooms code
      case mRoom of
        Nothing -> WS.sendTextData conn (encode (RoomError "Ma phong khong ton tai"))
        Just stateVar -> do
          let pid = 0 -- TODO: cap phat id thuc theo so nguoi hien co
              profile = PlayerProfile name Warrior 1 0 0
          joinResult <- joinRoom stateVar pid profile
          case joinResult of
            Left err -> WS.sendTextData conn (encode (RoomError err))
            Right () -> do
              gs <- readTVarIO stateVar
              WS.sendTextData conn (encode (RoomJoined pid gs))
              forever $ do
                msgRaw <- WS.receiveData conn
                case decode msgRaw :: Maybe ClientMsg of
                  Just (InputMsg _ _ _) -> pure () -- TODO: day input vao hang doi xu ly cua GameLoop
                  Just LeaveRoom -> pure ()
                  _ -> pure ()
    _ -> WS.sendTextData conn (encode (RoomError "Message dau tien phai la JoinRoom"))