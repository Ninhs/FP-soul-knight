{-# LANGUAGE OverloadedStrings #-}
module Network where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, try)
import Control.Concurrent.MVar (tryPutMVar)
import Data.Aeson (encode, decode)

import Protocol
import Types.GameState

data NetworkConnection = NetworkConnection
  { initialGameState   :: GameState
  , latestGameState    :: MVar GameState
  , connectionFailure :: MVar String
  , sendClientMessage  :: ClientMsg -> IO ()
  }

connectToServer :: String -> Int -> String -> String
                 -> IO (Either String NetworkConnection)
connectToServer host port code name = do
  ready <- newEmptyMVar
  _ <- forkIO $ do
    result <- try (WS.runClient host port "/" $ \conn -> do
      WS.sendTextData conn (encode (JoinRoom code name))
      raw <- WS.receiveData conn
      case decode raw :: Maybe ServerMsg of
        Just (RoomJoined _ gs) -> do
          latest <- newMVar gs
          failure <- newEmptyMVar
          let sendMessage msg = do
                sendResult <- try (WS.sendTextData conn (encode msg))
                  :: IO (Either SomeException ())
                case sendResult of
                  Left err -> do
                    _ <- tryPutMVar failure (displayException err)
                    pure ()
                  Right () -> pure ()
              connection = NetworkConnection gs latest failure sendMessage
          putMVar ready (Right connection)
          receiveMessages conn latest failure
        Just (RoomError err) -> putMVar ready (Left err)
        _ -> putMVar ready (Left "Server returned an invalid join response"))
      :: IO (Either SomeException ())
    case result of
      Left err -> do
        _ <- tryPutMVar ready (Left (displayException err))
        pure ()
      Right () -> do
        _ <- tryPutMVar ready (Left "Connection closed before the room was joined")
        pure ()
  takeMVar ready

receiveMessages :: WS.Connection -> MVar GameState -> MVar String -> IO ()
receiveMessages conn latest failure = do
  result <- try loop :: IO (Either SomeException ())
  case result of
    Left err -> do
      _ <- tryPutMVar failure (displayException err)
      pure ()
    Right () -> do
      _ <- tryPutMVar failure "Server closed the connection"
      pure ()
  where
    loop = do
      raw <- WS.receiveData conn
      case decode raw :: Maybe ServerMsg of
        Just (StateUpdate gs) -> putOrSwap latest gs >> loop
        Just (RoomJoined _ gs) -> putOrSwap latest gs >> loop
        Just (RoomError err) -> do
          _ <- tryPutMVar failure err
          pure ()
        Nothing -> do
          _ <- tryPutMVar failure "Server sent an invalid message"
          pure ()

-- Ghi đè MVar (xóa giá trị cũ nếu có, ghi giá trị mới)
putOrSwap :: MVar a -> a -> IO ()
putOrSwap mvar val = do
  _ <- tryTakeMVar mvar
  putMVar mvar val