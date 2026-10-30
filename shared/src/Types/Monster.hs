{-# LANGUAGE DeriveGeneric #-}
module Types.Monster where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Types.Player (Stats)

data AttackType = Melee | Ranged 
  deriving (Eq, Show, Generic)
instance ToJSON AttackType
instance FromJSON AttackType

data BodyType = TankBody | GlassBody 
  deriving (Eq, Show, Generic)
instance ToJSON BodyType
instance FromJSON BodyType

data MonsterKind = Regular AttackType BodyType | Boss
  deriving (Eq, Show, Generic)
instance ToJSON MonsterKind
instance FromJSON MonsterKind

data Monster = Monster
  { monsterId :: Int
  , mPosition :: (Float, Float)
  , mStats    :: Stats
  , kind      :: MonsterKind
  , mCooldown :: Float
  } deriving (Show, Generic)
instance ToJSON Monster
instance FromJSON Monster