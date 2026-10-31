module Types
  ( ViewState(..)
  , ClientState(..)
  , World
  , Character(..)
  , CharacterStats(..)
  , MapSize(..)
  , EnemyKind(..)
  , EnemyData(..)
  , AllyData(..)
  , DamageText(..)
  , damageEvents
  , stepDamageTexts
  , ProjectileData(..)
  , ChestState(..)
  , ChestData(..)
  , ChestReward(..)
  , ShopItemKind(..)
  , UpgradeKind(..)
  , RoomId(..)
  , characterStats
  , mapDimensions
  , mapRewardGold
  , worldScale
  , toWorldPosition
  , toScreenPosition
  , enemyAsset
  , initialClientState
  ) where

import Animation (AnimKind(..), AnimState(..), SpriteSet(..), startAnim)
import Assets (AssetId(..), GameAssets)
import qualified Types.GameState as GS
import Types.GameState (GameState, initialGameState)
import MapLayout (MapSize(..), RoomId(..), mapDimensions, startPosition)

data ViewState
  = MainMenuView
  | LobbyView
  | CharacterSelectView
  | MapSelectView
  | TutorialView
  | InGameView
  | ShopView
  | UpgradeView
  | PausedView
  | ChestRewardView
  | ShopNoticeView
  | ConfirmMenuView
  | VictoryView
  | DefeatView
  deriving (Eq, Show)

data ClientState = ClientState
  { appAssets :: GameAssets
  , appView :: ViewState
  , sharedState :: GameState
  , mousePos :: (Float, Float)
  , playerPosition :: (Float, Float)
  , localMapState :: GS.MapState
  , localAttackCooldown :: Float
  , localMonsterAttackCooldowns :: [(Int, Float)]
  , clearedRooms :: [RoomId]
  , activatedRooms :: [RoomId]
  , currentRoom :: Maybe RoomId
  , bossDefeated :: Bool
  , selectedChar :: Character
  , selectedMapSize :: MapSize
  , goldCoins :: Int
  , gems :: Int
  , roomCode :: String
  , roomInput :: String
  , roomInputActive :: Bool
  , doorOpen :: Float             -- 0 = cửa sắt đóng, 1 = mở hẳn (chạy hoạt ảnh)
  , stageClock :: Float           -- đồng hồ để ô vàng nhấp nháy
  , playerName :: String          -- tên nhập ở lobby (khoá tài khoản trong DB)
  , nameInputActive :: Bool
  , isReady :: Bool
  , isMultiplayer :: Bool
  , onlinePlayerId :: Maybe Int   -- id server cấp khi đang chơi online
  , statusText :: String
  , playerPos :: (Float, Float)
  , enemies :: [EnemyData]
  , projectiles :: [ProjectileData]
  , treasureChest :: ChestData
  , playerLevel :: Int
  , playerExperience :: Int
  , playerHealth :: Float
  , playerMaxHealth :: Float
  , playerArmor :: Float
  , playerMaxArmor :: Float
  , playerEnergy :: Float
  , playerMaxEnergy :: Float
  , playerDamage :: Float
  , playerMoveSpeed :: Float
  , bossHealth :: Float
  , bossMaxHealth :: Float
  , inventoryVisible :: Bool
  , shopEquipmentTab :: Bool
  , shopInventory :: [(ShopItemKind, Int)]
  , shopRefreshes :: Int
  , upgradeRefreshes :: Int
  , healthUpgradeLevel :: Int
  , damageUpgradeLevel :: Int
  , speedUpgradeLevel :: Int
  , attackEffectPos :: (Float, Float)
  , attackEffectTimer :: Float
  , damageTexts :: [DamageText]   -- số sát thương bay lên khi đánh trúng quái
  , tutorialVisible :: Bool
  , isVictory :: Bool
  , noticeText :: String
  , returnView :: ViewState
  , noticeReturnView :: ViewState
  , playerAnim :: AnimState
  , playerFacingRight :: Bool
  , enemyCorpses :: [EnemyData]
  , allies :: [AllyData]   -- người chơi khác khi chơi online
  , bossAnim :: AnimState
  }

type World = ClientState

data Character = Swordsman | Archer | Knight | Bomber | Mage
  deriving (Eq, Show, Read, Enum, Bounded)

data CharacterStats = CharacterStats
  { characterMaxHp :: Float
  , characterDamage :: Float
  , characterDefense :: Float
  , characterMoveSpeed :: Float
  } deriving (Eq, Show)

data EnemyKind = MeleeEnemy | RangedEnemy | TankEnemy | HighDamageEnemy
  deriving (Eq, Show, Read, Enum, Bounded)

-- Đồng đội trong phòng online (vẽ bằng sprite, vị trí đã làm mượt)
data AllyData = AllyData
  { allyId :: Int
  , allyName :: String
  , allyPos :: (Float, Float)
  , allyAnim :: AnimState
  , allyFacingRight :: Bool
  , allySprite :: SpriteSet
  } deriving (Eq, Show)

data EnemyData = EnemyData
  { enemyKind :: EnemyKind
  , enemyPos :: (Float, Float)
  , enemyCurrentHp :: Float
  , enemyMaxHp :: Float
  , enemyIsAlive :: Bool
  , enemyId :: Int
  , enemyAnim :: AnimState
  , enemyFacingRight :: Bool
  , enemySprite :: SpriteSet
  } deriving (Eq, Show)

data ProjectileData = ProjectileData
  { projectilePos :: (Float, Float)
  } deriving (Eq, Show)

data ChestState = ChestClosed | ChestOpened | ChestClaimed
  deriving (Eq, Show)

data ChestData = ChestData
  { chestPosition :: (Float, Float)
  , chestState :: ChestState
  } deriving (Eq, Show)

data ChestReward = ChestReward
  { rewardGold :: Int
  , rewardHealth :: Float
  , rewardGems :: Int
  } deriving (Eq, Show)

data ShopItemKind
  = SmallHealthPotion
  | LightArmor
  | LargeHealthPotion
  | HeavyArmor
  | DamageCharm
  | SwiftBoots
  deriving (Eq, Show, Read, Enum, Bounded)

data UpgradeKind = MaxHealthUpgrade | DamageUpgrade | MoveSpeedUpgrade
  deriving (Eq, Show, Read, Enum, Bounded)

characterStats :: Character -> CharacterStats
characterStats Swordsman = CharacterStats 120 34 15 220
characterStats Archer = CharacterStats 90 30 5 250
characterStats Knight = CharacterStats 150 25 25 190
characterStats Bomber = CharacterStats 100 38 8 210
characterStats Mage = CharacterStats 85 42 3 230

mapRewardGold :: MapSize -> Int
mapRewardGold LargeMap = 800

worldScale :: Float
worldScale = 35

toWorldPosition :: (Float, Float) -> (Float, Float)
toWorldPosition (x, y) = (x / worldScale, y / worldScale)

toScreenPosition :: (Float, Float) -> (Float, Float)
toScreenPosition (x, y) = (x * worldScale, y * worldScale)

enemyAsset :: EnemyKind -> AssetId
enemyAsset MeleeEnemy = AssetOrc
enemyAsset RangedEnemy = AssetMonster
enemyAsset TankEnemy = AssetOrc
enemyAsset HighDamageEnemy = AssetDemon

initialClientState :: GameAssets -> ClientState
initialClientState assets =
  let stats = characterStats Swordsman
      initialPosition = startPosition SmallMap
      initialPlayer = GS.Player
        { GS.playerId = 1
        , GS.position = toWorldPosition initialPosition
        , GS.stats = GS.Stats
            { GS.hp = round (characterMaxHp stats)
            , GS.maxHp = round (characterMaxHp stats)
            , GS.damage = round (characterDamage stats)
            , GS.moveSpeed = characterMoveSpeed stats / worldScale
            , GS.atkSpeed = 0.45
            , GS.atkRange = 3.0
            }
        , GS.status = GS.Alive
        , GS.pendingExp = 0
        , GS.pendingGold = 0
        }
  in ClientState
    { appAssets = assets
    , appView = MainMenuView
    , sharedState = initialGameState
    , mousePos = (0, 0)
    , playerPosition = initialPosition
    , localMapState = GS.MapState
        { GS.mapIndex = 1
        , GS.isBossMap = False
        , GS.players = [initialPlayer]
        , GS.monsters = []
        , GS.bullets = []
        , GS.drops = []
        , GS.portal = GS.Portal False (0, 0)
        }
    , localAttackCooldown = 0
    , localMonsterAttackCooldowns = []
    , clearedRooms = []
    , activatedRooms = []
    , currentRoom = Just StartRoom
    , bossDefeated = False
    , selectedChar = Swordsman
    , selectedMapSize = SmallMap
    , goldCoins = 500
    , gems = 10
    , roomCode = "LOCAL-01"
    , roomInput = ""
    , roomInputActive = False
    , doorOpen = 1
    , stageClock = 0
    , playerName = ""
    , nameInputActive = False
    , isReady = False
    , isMultiplayer = False
    , onlinePlayerId = Nothing
    , statusText = "Choose a mode to begin."
    , playerPos = initialPosition
    , enemies = []
    , projectiles = []
    , treasureChest = ChestData (250, 150) ChestClosed
    , playerLevel = 1
    , playerExperience = 0
    , playerHealth = characterMaxHp stats
    , playerMaxHealth = characterMaxHp stats
    , playerArmor = characterDefense stats
    , playerMaxArmor = characterDefense stats
    , playerEnergy = 100
    , playerMaxEnergy = 100
    , playerDamage = characterDamage stats
    , playerMoveSpeed = characterMoveSpeed stats
    , bossHealth = 300
    , bossMaxHealth = 300
    , inventoryVisible = False
    , shopEquipmentTab = False
    , shopInventory = []
    , shopRefreshes = 0
    , upgradeRefreshes = 0
    , healthUpgradeLevel = 0
    , damageUpgradeLevel = 0
    , speedUpgradeLevel = 0
    , attackEffectPos = (0, 0)
    , attackEffectTimer = 0
    , damageTexts = []
    , tutorialVisible = False
    , isVictory = False
    , noticeText = ""
    , returnView = MainMenuView
    , noticeReturnView = MainMenuView
    , playerAnim = startAnim Idle
    , playerFacingRight = True
    , enemyCorpses = []
    , allies = []
    , bossAnim = startAnim Idle
    }


-- Số sát thương nổi lên trên đầu quái (toạ độ màn hình).
data DamageText = DamageText
  { dtPos :: (Float, Float)
  , dtAmount :: Int
  , dtAge :: Float
  } deriving (Show)

-- So sánh quái trước / sau một khung hình: quái mất máu (hoặc biến mất) = vừa bị đánh.
-- Chỉ so khi cùng một màn; sang màn mới thì không có gì để so.
damageEvents :: GS.MapState -> GS.MapState -> [DamageText]
damageEvents before after
  | GS.mapIndex before /= GS.mapIndex after = []
  | otherwise = lost ++ gone
  where
    nowIds = map GS.monsterId (GS.monsters after)
    lost =
      [ DamageText (toScreenPosition (GS.mPosition m)) (h0 - GS.hp (GS.mStats m)) 0
      | m <- GS.monsters after
      , Just h0 <- [lookup (GS.monsterId m) prevHp]
      , GS.hp (GS.mStats m) < h0
      ]
    gone =
      [ DamageText (toScreenPosition (GS.mPosition m)) (GS.hp (GS.mStats m)) 0
      | m <- GS.monsters before
      , GS.monsterId m `notElem` nowIds
      , GS.hp (GS.mStats m) > 0
      ]
    prevHp = [(GS.monsterId m, GS.hp (GS.mStats m)) | m <- GS.monsters before]

-- Thêm số mới, tăng tuổi, bỏ số đã bay hết (0.8 giây).
stepDamageTexts :: Float -> [DamageText] -> [DamageText] -> [DamageText]
stepDamageTexts dt fresh old =
  [ t { dtAge = dtAge t + dt } | t <- old ++ fresh, dtAge t + dt < 0.8 ]
