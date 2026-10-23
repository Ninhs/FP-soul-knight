{-# LANGUAGE OverloadedStrings #-}
module Network where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Aeson (encode, decode)

import Protocol
import Types.GameState

-- Kết nối tới server, trả về MVar chứa GameState mới nhất (để Render.hs đọc)
-- và hàm gửi input lên server
connectToServer :: String -> Int -> String -> String
                 -> IO (MVar GameState, ClientMsg -> IO ())
connectToServer host port code name = do
  latestState <- newEmptyMVar
  connVar <- newEmptyMVar

  _ <- forkIO $ WS.runClient host port "/" $ \conn -> do
    putMVar connVar conn
    WS.sendTextData conn (encode (JoinRoom code name))
    forever $ do
      raw <- WS.receiveData conn
      case decode raw :: Maybe ServerMsg of
        Just (RoomJoined _ gs)   -> putOrSwap latestState gs
        Just (StateUpdate gs)    -> putOrSwap latestState gs
        Just (RoomError err)     -> putStrLn ("Server bao loi: " ++ err)
        Nothing                  -> pure ()

  conn <- readMVar connVar
  let sendInput msg = WS.sendTextData conn (encode msg)
  pure (latestState, sendInput)

-- Ghi đè MVar (xóa giá trị cũ nếu có, ghi giá trị mới)
putOrSwap :: MVar a -> a -> IO ()
putOrSwap mvar val = do
  _ <- tryTakeMVar mvar
  putMVar mvar val