module Assets
  ( AssetFit(..)
  , AssetSlot(..)
  , AssetStatus(..)
  , assetManifest
  , assetSlotLayout
  , assetStatus
  , loadAssetImages
  , drawAssetAt
  , drawAssetSlot
  , menuBackground
  , characterAsset
  , enemyAsset
  , mapAsset
  ) where

import Graphics.Gloss (Picture(..), loadBMP)
import System.Directory (doesFileExist)

data AssetFit = Contain | Cover | Stretch
  deriving (Eq, Show)

data AssetStatus = AssetLoaded | AssetPending FilePath
  deriving (Eq, Show)

data AssetDefinition = AssetDefinition
  { definitionPath :: FilePath
  , definitionSize :: (Float, Float)
  }

data AssetSlot = AssetSlot
  { slotId :: AssetSlotId
  , slotAsset :: AssetId
  , slotScreen :: Screen
  , slotPosition :: (Float, Float)
  , slotSize :: (Float, Float)
  , slotFit :: AssetFit
  }
  deriving (Eq, Show)

assetManifest :: [(AssetId, FilePath, (Float, Float))]
assetManifest =
  [ (AssetBackground, "assets/background.bmp", (1280, 720))
  , (AssetGameMap, "assets/map.bmp", (2752, 1536))
  , (AssetMapForest, "assets/maps/forest.bmp", (1280, 720))
  , (AssetMapDungeon, "assets/map.bmp", (2752, 1536))
  , (AssetMapBoss, "assets/maps/boss.bmp", (1280, 720))
  , (AssetGameLogo, "assets/ui/game_logo.bmp", (512, 160))
  , (AssetPlayerSwordsman, "assets/characters/player_swordsman.bmp", (128, 128))
  , (AssetPlayerArcher, "assets/characters/player_archer.bmp", (128, 128))
  , (AssetPlayerKnight, "assets/characters/player_knight.bmp", (128, 128))
  , (AssetPlayerBomber, "assets/characters/player_bomber.bmp", (128, 128))
  , (AssetPlayerMage, "assets/characters/player_mage.bmp", (128, 128))
  , (AssetMonsterMelee, "assets/monsters/monster_melee.bmp", (128, 128))
  , (AssetMonsterRanged, "assets/monsters/monster_ranged.bmp", (128, 128))
  , (AssetMonsterTank, "assets/monsters/monster_tank.bmp", (128, 128))
  , (AssetMonsterAssassin, "assets/monsters/monster_assassin.bmp", (128, 128))
  , (AssetBoss, "assets/monsters/boss.bmp", (256, 256))
  , (AssetChest, "assets/items/chest.bmp", (96, 96))
  , (AssetPortal, "assets/maps/portal.bmp", (128, 128))
  , (AssetCoin, "assets/items/coin_icon.bmp", (64, 64))
  , (AssetGem, "assets/items/gem_icon.bmp", (64, 64))
  , (AssetHealth, "assets/ui/health_icon.bmp", (64, 64))
  , (AssetArmor, "assets/ui/armor_icon.bmp", (64, 64))
  , (AssetSkill, "assets/ui/skill_icon.bmp", (64, 64))
  , (AssetSettings, "assets/ui/settings_icon.bmp", (64, 64))
  , (AssetProjectile, "assets/effects/projectile.bmp", (32, 32))
  , (AssetAttackEffect, "assets/effects/attack_effect.bmp", (128, 128))
  , (AssetSkillEffect, "assets/effects/skill_effect.bmp", (192, 192))
  , (AssetButtonStart, "assets/ui/button_start.bmp", (320, 80))
  , (AssetButtonBack, "assets/ui/button_back.bmp", (240, 72))
  , (AssetAvatarFrame, "assets/ui/avatar_frame.bmp", (160, 160))
  , (AssetMinimapFrame, "assets/ui/minimap_frame.bmp", (220, 170))
  , (AssetShopkeeper, "assets/characters/shopkeeper.bmp", (256, 256))
  , (AssetHealthPotion, "assets/items/health_potion.bmp", (96, 96))
  , (AssetLargePotion, "assets/items/large_potion.bmp", (96, 96))
  , (AssetLightArmor, "assets/items/light_armor.bmp", (96, 96))
  , (AssetHeavyArmor, "assets/items/heavy_armor.bmp", (96, 96))
  , (AssetDamageRune, "assets/items/damage_rune.bmp", (96, 96))
  , (AssetSwiftBoots, "assets/items/swift_boots.bmp", (96, 96))
  ]

assetSlotLayout :: [AssetSlot]
assetSlotLayout =
  [ slot SlotMenuBackground AssetBackground MenuScreen (0, 0) (1280, 720) Cover
  , slot SlotMenuLogo AssetGameLogo MenuScreen (-500, 300) (200, 100) Contain
  , slot SlotMenuCoin AssetCoin MenuScreen (430, 320) (34, 34) Contain
  , slot SlotMenuGem AssetGem MenuScreen (430, 280) (34, 34) Contain
  , slot SlotMenuSettings AssetSettings MenuScreen (590, 305) (38, 38) Contain
  , slot SlotMenuShopkeeper AssetShopkeeper MenuScreen (390, -165) (210, 210) Contain
  , slot SlotCharacterSwordsman AssetPlayerSwordsman CharSelectScreen (-375, -75) (64, 64) Contain
  , slot SlotCharacterArcher AssetPlayerArcher CharSelectScreen (-225, -75) (64, 64) Contain
  , slot SlotCharacterSelected AssetPlayerSwordsman CharSelectScreen (-300, 105) (230, 210) Contain
  , slot SlotLobbyAvatar1 AssetPlayerSwordsman LobbyScreen (-390, 5) (120, 135) Contain
  , slot SlotLobbyAvatar2 AssetPlayerArcher LobbyScreen (-130, 5) (120, 135) Contain
  , slot SlotLobbyAvatar3 AssetPlayerKnight LobbyScreen (130, 5) (120, 135) Contain
  , slot SlotLobbyAvatar4 AssetAvatarFrame LobbyScreen (390, 5) (120, 135) Contain
  , slot SlotMapThumbnail AssetGameMap MapSelectScreen (-300, 130) (440, 280) Contain
  , slot SlotHudAvatar AssetPlayerSwordsman GameHUDScreen (-555, 300) (58, 58) Contain
  , slot SlotHudMinimap AssetMinimapFrame GameHUDScreen (390, 165) (200, 160) Contain
  , slot SlotHudCoin AssetCoin GameHUDScreen (365, 326) (30, 30) Contain
  , slot SlotHudHealth AssetHealth GameHUDScreen (-510, 320) (28, 28) Contain
  , slot SlotHudArmor AssetArmor GameHUDScreen (-510, 283) (24, 24) Contain
  , slot SlotHudSkill1 AssetSkill GameHUDScreen (400, -285) (48, 48) Contain
  , slot SlotHudSkill2 AssetSkill GameHUDScreen (490, -285) (48, 48) Contain
  , slot SlotRewardChest AssetChest RewardScreen (0, 55) (160, 160) Contain
  , slot SlotRewardCoin AssetCoin RewardScreen (-150, -75) (48, 48) Contain
  , slot SlotRewardHealth AssetHealth RewardScreen (0, -75) (48, 48) Contain
  , slot SlotRewardArmor AssetArmor RewardScreen (150, -75) (48, 48) Contain
  , slot SlotShopkeeper AssetShopkeeper ShopScreen (-450, 105) (180, 200) Contain
  , slot SlotShopItem1 AssetHealthPotion ShopScreen (-270, 100) (76, 76) Contain
  , slot SlotShopItem2 AssetLightArmor ShopScreen (-50, 100) (76, 76) Contain
  , slot SlotShopItem3 AssetLargePotion ShopScreen (170, 100) (76, 76) Contain
  , slot SlotShopItem4 AssetHeavyArmor ShopScreen (390, 100) (76, 76) Contain
  , slot SlotUpgrade1 AssetHealth UpgradeScreen (-385, 155) (56, 56) Contain
  , slot SlotUpgrade2 AssetArmor UpgradeScreen (95, 155) (56, 56) Contain
  , slot SlotUpgrade3 AssetSkill UpgradeScreen (-385, -50) (56, 56) Contain
  , slot SlotUpgrade4 AssetSkill UpgradeScreen (95, -50) (56, 56) Contain
  , slot SlotCombatEnemy1 AssetMonsterMelee GameHUDScreen (120, 100) (64, 64) Contain
  , slot SlotCombatEnemy2 AssetMonsterRanged GameHUDScreen (220, 100) (64, 64) Contain
  , slot SlotCombatEnemy3 AssetMonsterTank GameHUDScreen (320, 100) (64, 64) Contain
  , slot SlotCombatEnemy4 AssetMonsterAssassin GameHUDScreen (420, 100) (64, 64) Contain
  ]
  where
    slot = AssetSlot

loadAssetImages :: IO [(AssetId, Picture)]
loadAssetImages = loadExisting assetManifest
  where
    loadExisting [] = pure []
    loadExisting ((assetId, path, _) : rest) = do
      exists <- doesFileExist path
      loaded <- if exists
        then do
          picture <- loadBMP path
          pure [(assetId, picture)]
        else pure []
      remaining <- loadExisting rest
      pure (loaded ++ remaining)

assetStatus :: [(AssetId, Picture)] -> AssetId -> AssetStatus
assetStatus images assetId =
  case lookup assetId images of
    Just _ -> AssetLoaded
    Nothing -> case lookupAssetDefinition assetId of
      Just definition -> AssetPending (definitionPath definition)
      Nothing -> AssetPending ""

drawAssetSlot :: World -> AssetSlotId -> Picture
drawAssetSlot w requestedSlot =
  case findSlot requestedSlot assetSlotLayout of
    Just slotSpec ->
      drawAssetAt w (slotAsset slotSpec) (slotPosition slotSpec)
        (slotSize slotSpec) (slotFit slotSpec)
    Nothing -> error ("Unknown asset slot: " ++ show requestedSlot)

drawAssetAt
  :: World
  -> AssetId
  -> (Float, Float)
  -> (Float, Float)
  -> AssetFit
  -> Picture
drawAssetAt w assetId (x, y) (targetWidth, targetHeight) fit =
  case lookup assetId (assetImages w) of
    Nothing -> Blank
    Just picture ->
      case lookupAssetDefinition assetId of
        Nothing -> picture
        Just definition ->
          let (sourceWidth, sourceHeight) = definitionSize definition
              scaleX = targetWidth / sourceWidth
              scaleY = targetHeight / sourceHeight
              (finalX, finalY) = case fit of
                Contain -> (min scaleX scaleY, min scaleX scaleY)
                Cover -> (max scaleX scaleY, max scaleX scaleY)
                Stretch -> (scaleX, scaleY)
          in Translate x y $ Scale finalX finalY picture

menuBackground :: World -> Picture
menuBackground w = drawAssetSlot w SlotMenuBackground

characterAsset :: Character -> AssetId
characterAsset Swordsman = AssetPlayerSwordsman
characterAsset Archer = AssetPlayerArcher
characterAsset Knight = AssetPlayerKnight
characterAsset Bomber = AssetPlayerBomber
characterAsset Mage = AssetPlayerMage

enemyAsset :: EnemyKind -> AssetId
enemyAsset MeleeEnemy = AssetMonsterMelee
enemyAsset RangedEnemy = AssetMonsterRanged
enemyAsset TankEnemy = AssetMonsterTank
enemyAsset HighDamageEnemy = AssetMonsterAssassin

mapAsset :: MapSize -> AssetId
mapAsset SmallMap = AssetMapDungeon
mapAsset MediumMap = AssetMapDungeon
mapAsset LargeMap = AssetMapBoss

findSlot :: AssetSlotId -> [AssetSlot] -> Maybe AssetSlot
findSlot _ [] = Nothing
findSlot requested (candidate : rest)
  | slotId candidate == requested = Just candidate
  | otherwise = findSlot requested rest

lookupAssetDefinition :: AssetId -> Maybe AssetDefinition
lookupAssetDefinition requested = findDefinition assetManifest
  where
    findDefinition [] = Nothing
    findDefinition ((assetId, path, size) : rest)
      | assetId == requested =
          Just (AssetDefinition path size)
      | otherwise = findDefinition rest
