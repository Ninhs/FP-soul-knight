module Assets 
  ( GameAssets(..)
  , AssetId(..)
  , AssetSlot(..)
  , FitMode(..)
  , emptyGameAssets
  , loadAssets
  ) where

import Graphics.Gloss
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import Data.List (sort, isSuffixOf)
import System.FilePath ((</>))

data AssetId
  = AssetBackground
  | AssetDungeonMap
  | AssetSwordsman
  | AssetArcher
  | AssetKnight
  | AssetBomber
  | AssetMage
  | AssetMonster
  | AssetOrc
  | AssetDemon
  | AssetGem
  | AssetSettings
  | AssetAvatarFrame
  | AssetArmor
  | AssetAttackEffect
  | AssetBoss
  | AssetChest
  | AssetCoin
  | AssetDamageRune
  | AssetHealth
  | AssetHealthPotion
  | AssetHeavyArmor
  | AssetLargePotion
  | AssetLightArmor
  | AssetMinimapFrame
  | AssetPortal
  | AssetProjectile
  | AssetSkill
  | AssetSwiftBoots
  deriving (Eq, Show, Read, Enum, Bounded)

data AssetSlot
  = SlotHudArmor
  | SlotHudAvatar
  | SlotHudCoin
  | SlotHudHealth
  | SlotHudSkill1
  | SlotHudSkill2
  | SlotMenuCoin
  | SlotMenuGem
  | SlotMenuLogo
  | SlotMenuSettings
  | SlotMenuShopkeeper
  | SlotRewardChest
  | SlotShopkeeper
  deriving (Eq, Show, Read, Enum, Bounded)

data FitMode = Contain | Cover | Stretch
  deriving (Eq, Show, Read, Enum, Bounded)

-- Cấu trúc lưu trữ toàn bộ bộ ảnh cho 5 nhân vật
data GameAssets = GameAssets
  { backgroundImage :: Picture
  , dungeonMapImage :: Picture
  , archerIdle    :: [Picture], archerWalk    :: [Picture], archerAttack    :: [Picture], archerDie    :: [Picture]
  , demonIdle     :: [Picture], demonWalk     :: [Picture], demonAttack     :: [Picture], demonDie     :: [Picture]
  , monsterIdle   :: [Picture], monsterWalk   :: [Picture], monsterAttack   :: [Picture], monsterDie   :: [Picture]
  , orcIdle       :: [Picture], orcWalk       :: [Picture], orcAttack       :: [Picture], orcDie       :: [Picture]
  , soldierIdle   :: [Picture], soldierWalk   :: [Picture], soldierAttack   :: [Picture], soldierDie   :: [Picture]
  , coinBagImage :: Picture
  , goldChestImage :: Picture
  , blueShieldImage :: Picture
  , manaBarImage :: Picture
  , hotbarSlotImage :: Picture
  } deriving (Show)

emptyGameAssets :: GameAssets
emptyGameAssets = GameAssets
  { backgroundImage = Blank, dungeonMapImage = Blank
  , archerIdle = [], archerWalk = [], archerAttack = [], archerDie = []
  , demonIdle = [], demonWalk = [], demonAttack = [], demonDie = []
  , monsterIdle = [], monsterWalk = [], monsterAttack = [], monsterDie = []
  , orcIdle = [], orcWalk = [], orcAttack = [], orcDie = []
  , soldierIdle = [], soldierWalk = [], soldierAttack = [], soldierDie = []
  , coinBagImage = Blank, goldChestImage = Blank, blueShieldImage = Blank
  , manaBarImage = Blank, hotbarSlotImage = Blank
  }

-- Hàm quét thư mục an toàn
loadFolder :: FilePath -> IO [Picture]
loadFolder dir = do
  exists <- doesDirectoryExist dir
  if not exists 
    then return []
    else do
      files <- listDirectory dir
      let bmpFiles = sort (filter (".bmp" `isSuffixOf`) files)
      sequence [ loadBMP (dir </> f) | f <- bmpFiles ]

loadOptionalBMP :: FilePath -> IO Picture
loadOptionalBMP path = do
  exists <- doesFileExist path
  if exists then loadBMP path else pure Blank

-- Load toàn bộ tài nguyên
loadAssets :: IO GameAssets
loadAssets = do
  clientAssetsExist <- doesDirectoryExist "client/assets"
  let assetRoot = if clientAssetsExist then "client/assets" else "assets"
      assetFolder character animation = assetRoot </> character </> animation
  aIdle   <- loadFolder (assetFolder "archer" "idle")
  aWalk   <- loadFolder (assetFolder "archer" "walk")
  aAttack <- loadFolder (assetFolder "archer" "attack")
  aDie    <- loadFolder (assetFolder "archer" "die")

  dIdle   <- loadFolder (assetFolder "demon" "idle")
  dWalk   <- loadFolder (assetFolder "demon" "walk")
  dAttack <- loadFolder (assetFolder "demon" "attack")
  dDie    <- loadFolder (assetFolder "demon" "die")

  mIdle   <- loadFolder (assetFolder "monster" "idle")
  mWalk   <- loadFolder (assetFolder "monster" "walk")
  mAttack <- loadFolder (assetFolder "monster" "attack")
  mDie    <- loadFolder (assetFolder "monster" "die")

  oIdle   <- loadFolder (assetFolder "orc" "idle")
  oWalk   <- loadFolder (assetFolder "orc" "walk")
  oAttack <- loadFolder (assetFolder "orc" "attack")
  oDie    <- loadFolder (assetFolder "orc" "die")

  sIdle   <- loadFolder (assetFolder "soldier" "idle")
  sWalk   <- loadFolder (assetFolder "soldier" "walk")
  sAttack <- loadFolder (assetFolder "soldier" "attack")
  sDie    <- loadFolder (assetFolder "soldier" "die")
  background <- loadOptionalBMP (assetRoot </> "background.bmp")
  dungeonMap <- loadOptionalBMP (assetRoot </> "map.bmp")
  coinBag <- loadOptionalBMP (assetRoot </> "ui" </> "coin_bag.bmp")
  goldChest <- loadOptionalBMP (assetRoot </> "ui" </> "chest_gold.bmp")
  blueShield <- loadOptionalBMP (assetRoot </> "ui" </> "shield_kite_blue.bmp")
  manaBar <- loadOptionalBMP (assetRoot </> "ui" </> "bar_fill_mana.bmp")
  hotbarSlot <- loadOptionalBMP (assetRoot </> "ui" </> "hotbar_slot.bmp")

  return GameAssets
    { backgroundImage = background, dungeonMapImage = dungeonMap
    , archerIdle    = aIdle,   archerWalk    = aWalk,   archerAttack    = aAttack,   archerDie    = aDie
    , demonIdle     = dIdle,   demonWalk     = dWalk,   demonAttack     = dAttack,   demonDie     = dDie
    , monsterIdle   = mIdle,   monsterWalk   = mWalk,   monsterAttack   = mAttack,   monsterDie   = mDie
    , orcIdle       = oIdle,   orcWalk       = oWalk,   orcAttack       = oAttack,   orcDie       = oDie
    , soldierIdle   = sIdle,   soldierWalk   = sWalk,   soldierAttack   = sAttack,   soldierDie   = sDie
    , coinBagImage = coinBag, goldChestImage = goldChest, blueShieldImage = blueShield
    , manaBarImage = manaBar, hotbarSlotImage = hotbarSlot
    }