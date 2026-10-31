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
  , metaArcherHpLv    :: Int   -- cấp nâng cấp riêng của Archer (3 cột trên là của Warrior)
  , metaArcherDmgLv   :: Int
  , metaArcherSpeedLv :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON Meta
instance FromJSON Meta

defaultMeta :: Meta
defaultMeta = Meta 10 0 0 0 0 0 0

-- Cấp (máu, sát thương, tốc chạy) của từng nghề.
metaLevels :: Role -> Meta -> (Int, Int, Int)
metaLevels Warrior m = (metaHpLv m, metaDmgLv m, metaSpeedLv m)
metaLevels Archer m = (metaArcherHpLv m, metaArcherDmgLv m, metaArcherSpeedLv m)

setMetaLevels :: Role -> (Int, Int, Int) -> Meta -> Meta
setMetaLevels Warrior (h, d, s) m = m { metaHpLv = h, metaDmgLv = d, metaSpeedLv = s }
setMetaLevels Archer (h, d, s) m = m { metaArcherHpLv = h, metaArcherDmgLv = d, metaArcherSpeedLv = s }

-- Cộng chỉ số nâng cấp vào chỉ số gốc của nghề (tốc chạy tính theo đơn vị thế giới: px / 35).
applyMeta :: Role -> Meta -> Stats -> Stats
applyMeta role m st = st
  { maxHp = maxHp st + 20 * hl
  , hp = hp st + 20 * hl
  , damage = damage st + 5 * dl
  , moveSpeed = moveSpeed st + fromIntegral (10 * sl) / 35
  }
  where (hl, dl, sl) = metaLevels role m

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