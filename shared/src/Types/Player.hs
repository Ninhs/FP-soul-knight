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

-- Dữ liệu lưu lâu dài trong database: gem và cấp nâng cấp (mỗi cấp: +20 máu / +5 sát thương / +10 tốc chạy).
data Meta = Meta
  { metaGems    :: Int
  , metaHpLv    :: Int
  , metaDmgLv   :: Int
  , metaSpeedLv :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON Meta
instance FromJSON Meta

defaultMeta :: Meta
defaultMeta = Meta 10 0 0 0

-- Cộng chỉ số nâng cấp vào chỉ số gốc của nghề (tốc chạy tính theo đơn vị thế giới: px / 35).
applyMeta :: Meta -> Stats -> Stats
applyMeta m st = st
  { maxHp = maxHp st + 20 * metaHpLv m
  , hp = hp st + 20 * metaHpLv m
  , damage = damage st + 5 * metaDmgLv m
  , moveSpeed = moveSpeed st + fromIntegral (10 * metaSpeedLv m) / 35
  }

data PlayerProfile = PlayerProfile
  { profileName :: String
  , profileRole :: Role
  , profileLv   :: Int
  , profileExp  :: Int
  , profileGold :: Int
  , profileReady :: Bool   -- đã bấm READY UP (chủ phòng không cần)
  , profileMeta :: Meta    -- gem + cấp nâng cấp đã lưu
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