{-# LANGUAGE DeriveGeneric #-}
module Types.Item where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data BulletOwner = FromPlayer Int | FromMonster Int 
  deriving (Eq, Show, Generic)
instance ToJSON BulletOwner
instance FromJSON BulletOwner

data Bullet = Bullet
  { bPosition :: (Float, Float)
  , bVelocity :: (Float, Float)
  , bOwner    :: BulletOwner
  , bDamage   :: Int
  , bLife     :: Float
  } deriving (Show, Generic)
instance ToJSON Bullet
instance FromJSON Bullet

data DropType = GoldDrop Int | ExpDrop Int | HealDrop Int
  deriving (Show, Generic)
instance ToJSON DropType
instance FromJSON DropType

data Drop = Drop
  { dPosition :: (Float, Float)
  , dType     :: DropType
  } deriving (Show, Generic)
instance ToJSON Drop
instance FromJSON Drop