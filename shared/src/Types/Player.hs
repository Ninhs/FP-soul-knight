{-# LANGUAGE DeriveGeneric #-}
module Types.Player where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Role = Warrior | Archer
  deriving (Eq, Show, Generic)
instance ToJSON Role
instance FromJSON Role

data Stats = Stats
  { hp        :: Int
  , maxHp     :: Int
  , damage    :: Int
  , moveSpeed :: Float
  , atkSpeed  :: Float
  , atkRange  :: Float
  } deriving (Show, Generic)
instance ToJSON Stats
instance FromJSON Stats

data PlayerStatus = Alive | Dead | Disconnected
  deriving (Eq, Show, Generic)
instance ToJSON PlayerStatus
instance FromJSON PlayerStatus

data PlayerProfile = PlayerProfile
  { profileName :: String
  , profileRole :: Role
  , profileLv   :: Int
  , profileExp  :: Int
  , profileGold :: Int
  } deriving (Show, Generic)
instance ToJSON PlayerProfile
instance FromJSON PlayerProfile

data Player = Player
  { playerId    :: Int
  , position    :: (Float, Float)
  , stats       :: Stats
  , status      :: PlayerStatus
  , pendingExp  :: Int
  , pendingGold :: Int
  } deriving (Show, Generic)
instance ToJSON Player
instance FromJSON Player