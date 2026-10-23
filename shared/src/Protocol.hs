{-# LANGUAGE DeriveGeneric #-}
module Protocol where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Types.GameState (GameState)

-- Những gì CLIENT gửi lên server
data ClientMsg
  = JoinRoom { roomCode :: String, playerName :: String }
  | InputMsg { moveDir :: (Float, Float), aimDir :: (Float, Float), attack :: Bool }
  | LeaveRoom
  deriving (Show, Generic)
instance ToJSON ClientMsg
instance FromJSON ClientMsg

-- Những gì SERVER gửi về client
data ServerMsg
  = RoomJoined { assignedId :: Int, initialState :: GameState }
  | StateUpdate GameState
  | RoomError String
  deriving (Show, Generic)
instance ToJSON ServerMsg
instance FromJSON ServerMsg