module Assets 
  ( GameAssets(..)
  , loadAssets
  ) where

import Graphics.Gloss
import System.Directory (listDirectory)
import Data.List (sort, isSuffixOf)
import System.FilePath ((</>))

-- Cấu trúc lưu trữ toàn bộ 20 bộ hoạt ảnh (5 nhân vật x 4 trạng thái)
data GameAssets = GameAssets
  { -- Archer
    archerIdle    :: [Picture], archerWalk    :: [Picture], archerAttack    :: [Picture], archerDie    :: [Picture]
  , -- Demon
    demonIdle     :: [Picture], demonWalk     :: [Picture], demonAttack     :: [Picture], demonDie     :: [Picture]
  , -- Monster
    monsterIdle   :: [Picture], monsterWalk   :: [Picture], monsterAttack   :: [Picture], monsterDie   :: [Picture]
  , -- Orc
    orcIdle       :: [Picture], orcWalk       :: [Picture], orcAttack       :: [Picture], orcDie       :: [Picture]
  , -- Soldier
    soldierIdle   :: [Picture], soldierWalk   :: [Picture], soldierAttack   :: [Picture], soldierDie   :: [Picture]
  } deriving (Show)

-- Hàm tự động quét và load tất cả file .bmp trong một thư mục
loadFolder :: FilePath -> IO [Picture]
loadFolder dir = do
  files <- listDirectory dir
  let bmpFiles = sort (filter (".bmp" `isSuffixOf`) files)
  sequence [ loadBMP (dir </> f) | f <- bmpFiles ]

-- Load toàn bộ asset game
loadAssets :: IO GameAssets
loadAssets = do
  -- Archer
  aIdle   <- loadFolder "assets/archer/idle"
  aWalk   <- loadFolder "assets/archer/walk"
  aAttack <- loadFolder "assets/archer/attack"
  aDie    <- loadFolder "assets/archer/die"

  -- Demon
  dIdle   <- loadFolder "assets/demon/idle"
  dWalk   <- loadFolder "assets/demon/walk"
  dAttack <- loadFolder "assets/demon/attack"
  dDie    <- loadFolder "assets/demon/die"

  -- Monster
  mIdle   <- loadFolder "assets/monster/idle"
  mWalk   <- loadFolder "assets/monster/walk"
  mAttack <- loadFolder "assets/monster/attack"
  mDie    <- loadFolder "assets/monster/die"

  -- Orc
  oIdle   <- loadFolder "assets/orc/idle"
  oWalk   <- loadFolder "assets/orc/walk"
  oAttack <- loadFolder "assets/orc/attack"
  oDie    <- loadFolder "assets/orc/die"

  -- Soldier
  sIdle   <- loadFolder "assets/soldier/idle"
  sWalk   <- loadFolder "assets/soldier/walk"
  sAttack <- loadFolder "assets/soldier/attack"
  sDie    <- loadFolder "assets/soldier/die"

  return GameAssets
    { archerIdle    = aIdle,   archerWalk    = aWalk,   archerAttack    = aAttack,   archerDie    = aDie
    , demonIdle     = dIdle,   demonWalk     = dWalk,   demonAttack     = dAttack,   demonDie     = dDie
    , monsterIdle   = mIdle,   monsterWalk   = mWalk,   monsterAttack   = mAttack,   monsterDie   = mDie
    , orcIdle       = oIdle,   orcWalk       = oWalk,   orcAttack       = oAttack,   orcDie       = oDie
    , soldierIdle   = sIdle,   soldierWalk   = sWalk,   soldierAttack   = sAttack,   soldierDie   = sDie
    }