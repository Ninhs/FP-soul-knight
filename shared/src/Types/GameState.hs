{-# LANGUAGE DeriveGeneric #-}
module Types.GameState where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Role & Stats

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

-- Player

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

-- Monster

data AttackType = Melee | Ranged deriving (Eq, Show, Generic)
instance ToJSON AttackType
instance FromJSON AttackType

data BodyType = TankBody | GlassBody deriving (Eq, Show, Generic)
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

-- Bullet, Drop, Portal

data BulletOwner = FromPlayer Int | FromMonster Int
  deriving (Show, Generic)
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

data Portal = Portal
  { portalActive :: Bool
  , portalPos    :: (Float, Float)
  } deriving (Show, Generic)
instance ToJSON Portal
instance FromJSON Portal

-- Map template (bản thiết kế map, dùng để random / cố định boss)

data MapTemplate = MapTemplate
  { templateIsBoss :: Bool
  , spawnPoints    :: [(Float, Float)]
  , portalSpawn    :: (Float, Float)
  , monsterPool    :: [MonsterKind]
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


