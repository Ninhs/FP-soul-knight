{-# LANGUAGE DeriveGeneric #-}
module Types.GameState where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Portal = Portal
  { portalActive :: Bool
  , portalPos    :: (Float, Float)
  } deriving (Show, Generic)
instance ToJSON Portal
instance FromJSON Portal

-- Map template (bản thiết kế map, dùng để random / cố định boss)

data MapTemplate = MapTemplate
  { portalSpawn :: (Float, Float)
  , mapBounds   :: (Float, Float, Float, Float)  -- (minX, maxX, minY, maxY)
  } deriving (Show, Generic)
instance ToJSON MapTemplate
instance FromJSON MapTemplate

-- Trạng thái map đang chơi

data MapState = MapState
  { mapIndex  :: Int
  , isBossMap :: Bool
  , players   :: [Player]
  , monsters  :: [Monster]
  , bullets   :: [Bullet]
  , drops     :: [Drop]
  , portal    :: Portal
  } deriving (Show, Generic)
instance ToJSON MapState
instance FromJSON MapState

-- Trạng thái toàn phòng

data Result = Victory | Defeat deriving (Eq, Show, Generic)
instance ToJSON Result
instance FromJSON Result

data RoomPhase
  = Lobby
  | Playing MapState
  | RoomResult Result
  deriving (Show, Generic)
instance ToJSON RoomPhase
instance FromJSON RoomPhase

data GameState = GameState
  { phase    :: RoomPhase
  , profiles :: [(Int, PlayerProfile)]
  } deriving (Show, Generic)
instance ToJSON GameState
instance FromJSON GameState


