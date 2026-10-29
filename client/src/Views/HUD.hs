module Views.HUD
  ( drawChestRewardOverlay
  , drawGameHUD
  , drawTutorialGuide
  , mapExit
  ) where

import Graphics.Gloss
import Assets
import Types
import Components.UI
  ( drawSoul3DText
  , drawSoulButton
  , drawTextShadow
  , drawPanel
  , drawHealthBar
  , drawEnergyBar
  , drawArmorBar
  , drawCoinCounter
  , drawInfoPanel
  , insideRect
  )
import qualified Theme

drawGameHUD :: World -> Picture
drawGameHUD w = Pictures
  [ Translate (-px) (-py) $ Pictures
      [ drawAssetAt w (mapAsset (selectedMapSize w)) (0, 0)
          (fromIntegral mapColumns * Theme.dungeonTileSize,
           fromIntegral mapRows * Theme.dungeonTileSize) Contain
      , if mapIndex w < 3 then drawPortal w (mapExit (selectedMapSize w)) else Blank
      , drawChest w
      , Pictures (map (drawEnemy w) (enemies w))
      , Pictures (map (drawProjectile w) (projectiles w))
      , drawBossIfPresent w
      , drawPlayer w
      , drawAttackEffect w (attackEffectTimer w) (attackEffectPos w)
      ]
  , drawFixedHud w
  , if tutorialVisible w then drawTutorialOverlay w else Blank
  ]
  where
    (px, py) = playerPos w
    (mapColumns, mapRows) = mapDimensions (selectedMapSize w)

mapExit :: MapSize -> (Float, Float)
mapExit mapSize =
  let (columns, rows) = mapDimensions mapSize
      halfWidth = fromIntegral columns * Theme.dungeonTileSize / 2
      halfHeight = fromIntegral rows * Theme.dungeonTileSize / 2
  in (halfWidth - Theme.dungeonTileSize * 1.5,
      halfHeight - Theme.dungeonTileSize * 1.5)

drawPortal :: World -> (Float, Float) -> Picture
drawPortal w position =
  drawAssetAt w AssetPortal position (76, 76) Contain

drawChest :: World -> Picture
drawChest w = Pictures
  [ drawAssetAt w AssetChest (chestPosition chest) (54, 54) Contain
  , Translate x (y + 34) $
      drawTextShadow 0.11 Theme.cTextWhite Theme.cDarkBorder label
  ]
  where
    chest = treasureChest w
    (x, y) = chestPosition chest
    label = case chestState chest of
      ChestClosed -> "CHEST [E]"
      ChestOpened -> "OPEN"
      ChestClaimed -> "EMPTY"

drawEnemy :: World -> EnemyData -> Picture
drawEnemy w enemy
  | not (enemyIsAlive enemy) = Blank
  | otherwise = Translate x y $ Pictures
      [ drawAssetAt w (enemyAsset (enemyKind enemy)) (0, 0) (54, 64) Contain
      , Translate 0 42 $ enemyHealthBar enemy
      , Translate 0 (-42) $
          drawTextShadow 0.10 Theme.cTextWhite Theme.cDarkBorder
            (enemyLabel (enemyKind enemy))
      ]
  where
    (x, y) = enemyPos enemy

enemyHealthBar :: EnemyData -> Picture
enemyHealthBar enemy =
  drawHealthBar 48 7 (enemyCurrentHp enemy) (enemyMaxHp enemy)

drawProjectile :: World -> ProjectileData -> Picture
drawProjectile w projectile =
  drawAssetAt w AssetProjectile (projectilePos projectile) (24, 24) Contain

enemyLabel :: EnemyKind -> String
enemyLabel MeleeEnemy = "MELEE"
enemyLabel RangedEnemy = "RANGED"
enemyLabel TankEnemy = "TANK"
enemyLabel HighDamageEnemy = "ASSASSIN"

drawPlayer :: World -> Picture
drawPlayer w = Pictures
  [ drawAssetAt w (characterAsset (selectedChar w)) (playerPos w) (56, 68) Contain
  , Translate x (y + 43) $
      drawTextShadow 0.11 Theme.cTextWhite Theme.cDarkBorder
        (characterName (selectedChar w))
  ]
  where
    (x, y) = playerPos w

characterName :: Character -> String
characterName Swordsman = "SWORDSMAN"
characterName Archer = "ARCHER"
characterName Knight = "KNIGHT"
characterName Bomber = "BOMBER"
characterName Mage = "MAGE"

characterWeapon :: Character -> String
characterWeapon Archer = "BOW"
characterWeapon Bomber = "BOMBS"
characterWeapon Mage = "STAFF"
characterWeapon _ = "SWORD"

drawAttackEffect :: World -> Float -> (Float, Float) -> Picture
drawAttackEffect w duration position
  | duration <= 0 = Blank
  | otherwise = drawAssetAt w AssetAttackEffect position (100, 100) Contain

drawBossIfPresent :: World -> Picture
drawBossIfPresent w
  | mapIndex w /= 3 = Blank
  | otherwise = drawAssetAt w AssetBoss (250, 0) (150, 150) Contain

drawFixedHud :: World -> Picture
drawFixedHud w = Pictures
  [ Translate (-455) 310 $ drawPanel 370 104
  , drawAssetSlot w SlotHudAvatar
  , Translate (-400) 334 $
      drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder
        (characterName (selectedChar w) ++ "  LV " ++ show (playerLevel w))
  , drawAssetSlot w SlotHudHealth
  , Translate (-350) 304 $
      drawHealthBar 250 16 (playerHealth w) (playerMaxHealth w)
  , drawAssetSlot w SlotHudArmor
  , Translate (-350) 279 $
      drawArmorBar 250 10 (playerArmor w) (playerMaxArmor w)
  , Translate (-350) 258 $
      drawEnergyBar 250 8 (playerEnergy w) (playerMaxEnergy w)
  , Translate (-455) 236 $
      drawTextShadow 0.11 Theme.cAccentBlue Theme.cDarkBorder
        ("EXP " ++ show (playerExperience w) ++ " / " ++ show (playerLevel w * 100))
  , Translate 470 325 $ drawPanel 370 78
  , drawAssetSlot w SlotHudCoin
  , Translate 410 326 $ drawCoinCounter (goldCoins w)
  , Translate 390 296 $
      drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder
        ("STAGE " ++ show (mapIndex w) ++ " / 3")
  , drawMinimap w
  , Translate 560 320 $ drawSoulButton 130 42 "PAUSE"
      (insideRect (mousePos w) (560, 320) 130 42)
  , Translate 560 265 $ drawSoulButton 130 42 "SHOP"
      (insideRect (mousePos w) (560, 265) 130 42)
  , Translate 560 210 $ drawSoulButton 130 42 "UPGRADE"
      (insideRect (mousePos w) (560, 210) 130 42)
  , if inventoryVisible w then drawInventoryBar w else Blank
  , drawSkillSlots w
  , Translate (-455) (-322) $
      drawTextShadow 0.13 Theme.cTextWhite Theme.cDarkBorder
        "WASD MOVE   /   LEFT CLICK AIM + ATTACK   /   E INTERACT"
  , if mapIndex w == 3
      then Translate 0 325 $
        drawBossHPBar "BOSS" (bossHealth w) (bossMaxHealth w)
      else Blank
  ]

drawMinimap :: World -> Picture
drawMinimap w = Translate 390 165 $ Pictures
  [ drawPanel 190 140
  , drawAssetAt w AssetMinimapFrame (0, 0) (190, 140) Contain
  , drawAssetAt w (mapAsset (selectedMapSize w)) (0, 0) (130, 84) Contain
  , Translate 0 48 $
      drawSoul3DText 0.9 Theme.cTextGold Theme.cDarkBorder "MINIMAP SLOT"
  , Translate 0 (-47) $
      drawTextShadow 0.10 Theme.cTextMuted Theme.cDarkBorder
        ("X " ++ show (round (fst (playerPos w)) :: Int)
          ++ "   Y " ++ show (round (snd (playerPos w)) :: Int))
  ]

drawStatusPanel :: World -> Picture
drawStatusPanel w = Pictures
  [ drawPanel 330 90
  , Translate 0 24 $
      drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder
        (characterWeapon (selectedChar w) ++ "  DMG "
          ++ show (round (playerDamage w) :: Int))
  , Translate 0 (-12) $
      drawTextShadow 0.12 Theme.cTextMuted Theme.cDarkBorder
        ("MOVE SPEED " ++ show (round (playerMoveSpeed w) :: Int))
  , Translate 0 (-34) $
      drawTextShadow 0.11 Theme.cAccentBlue Theme.cDarkBorder
        (statusText w)
  ]

drawInventoryBar :: World -> Picture
drawInventoryBar w = Translate 25 (-255) $ Pictures
  [ drawPanel 360 74
  , Translate 0 25 $
      drawSoul3DText 0.9 Theme.cTextGold Theme.cDarkBorder "INVENTORY  [I]"
  , Pictures
      [ drawInventorySlot w index kind count x
      | (index, kind, x) <-
          [ (1, SmallHealthPotion, -132)
          , (2, LightArmor, -44)
          , (3, LargeHealthPotion, 44)
          , (4, HeavyArmor, 132)
          ]
      , let count = inventoryCount kind (shopInventory w)
      ]
  ]

drawInventorySlot :: World -> Int -> ShopItemKind -> Int -> Float -> Picture
drawInventorySlot w index kind count x = Translate x (-9) $ Pictures
  [ drawPanel 56 40
  , drawAssetAt w (itemAsset kind) (0, 0) (30, 30) Contain
  , Translate 20 14 $
      drawTextShadow 0.10 Theme.cTextGold Theme.cDarkBorder (show count)
  , Translate 0 (-18) $
      drawTextShadow 0.10 Theme.cTextWhite Theme.cDarkBorder (show index)
  ]

itemAsset :: ShopItemKind -> AssetId
itemAsset SmallHealthPotion = AssetHealthPotion
itemAsset LightArmor = AssetLightArmor
itemAsset LargeHealthPotion = AssetLargePotion
itemAsset HeavyArmor = AssetHeavyArmor
itemAsset DamageCharm = AssetDamageRune
itemAsset SwiftBoots = AssetSwiftBoots

inventoryCount :: ShopItemKind -> [(ShopItemKind, Int)] -> Int
inventoryCount kind inventory = sum
  [ count | (item, count) <- inventory, item == kind ]

drawSkillSlots :: World -> Picture
drawSkillSlots w = Pictures
  [ drawSkillSlot w (390, -285) "Q" "SKILL"
  , drawSkillSlot w (490, -285) "R" "ALT"
  ]

drawSkillSlot :: World -> (Float, Float) -> String -> String -> Picture
drawSkillSlot w position@(x, y) key label = Pictures
  [ Translate x y $ drawPanel 62 62
  , drawAssetAt w AssetSkill position (44, 44) Contain
  , Translate x (y - 40) $
      drawTextShadow 0.10 Theme.cTextWhite Theme.cDarkBorder (key ++ "  " ++ label)
  ]

drawBossHPBar :: String -> Float -> Float -> Picture
drawBossHPBar bossName currentHp maximumHp = Pictures
  [ Translate 0 19 $
      drawSoul3DText 1.25 Theme.cTextWhite Theme.cDarkBorder bossName
  , Translate 0 (-9) $ drawHealthBar 260 14 currentHp maximumHp
  ]

drawChestRewardOverlay :: World -> ChestReward -> (Float, Float) -> Picture
drawChestRewardOverlay w reward mouse = Pictures
  [ Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawInfoPanel 620 390 "RECEIVED"
      "Chest rewards are ready to collect."
  , drawAssetSlot w SlotRewardChest
  , rewardSlot w (-180, -85) AssetCoin
      ("COINS +" ++ show (rewardGold reward))
  , rewardSlot w (0, -85) AssetHealth
      ("HEALTH +" ++ show (round (rewardHealth reward) :: Int))
  , rewardSlot w (180, -85) AssetArmor
      ("ARMOR +" ++ show (round (rewardArmor reward) :: Int))
  , Translate 0 (-220) $ drawSoulButton 220 54 "TAKE ALL"
      (insideRect mouse (0, -220) 220 54)
  ]

rewardSlot :: World -> (Float, Float) -> AssetId -> String -> Picture
rewardSlot w (x, y) asset label = Pictures
  [ drawAssetAt w asset (x, y) (48, 48) Contain
  , Translate x (y - 42) $
      drawTextShadow 0.11 Theme.cTextWhite Theme.cDarkBorder label
  ]

drawTutorialOverlay :: World -> Picture
drawTutorialOverlay w = Pictures
  [ Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawTutorialPanel w
  , Translate 0 (-235) $ drawSoulButton 200 48 "CONTINUE"
      (insideRect (mousePos w) (0, -235) 200 48)
  ]

drawTutorialGuide :: World -> Picture
drawTutorialGuide w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawTutorialPanel w
  , Translate (-145) (-235) $ drawSoulButton 230 48 "PRACTICE RUN"
      (insideRect (mousePos w) (-145, -235) 230 48)
  , Translate 145 (-235) $ drawSoulButton 180 48 "BACK"
      (insideRect (mousePos w) (145, -235) 180 48)
  ]

drawTutorialPanel :: World -> Picture
drawTutorialPanel w = Pictures
  [ drawPanel 690 500
  , Translate 0 200 $
      drawSoul3DText 2.2 Theme.cTextGold Theme.cDarkBorder "FIELD GUIDE"
  , guideRow 115 "WASD" "Move in four directions or diagonally"
  , guideRow 65 "LEFT CLICK" "Aim with the pointer and attack"
  , guideRow 15 "Q / R" "Use the available skill slots"
  , guideRow (-35) "E" "Interact with nearby objects"
  , guideRow (-85) "I / 1-4" "Show inventory / use an item"
  , drawAssetSlot w SlotHudSkill1
  , drawAssetSlot w SlotHudSkill2
  , Translate (-285) (-150) $
      drawTextShadow 0.11 Theme.cTextMuted Theme.cDarkBorder
        "SKILL AND INVENTORY ASSET SLOTS"
  , Translate 0 (-190) $
      drawTextShadow 0.11 Theme.cAccentBlue Theme.cDarkBorder
        "MOCK ENEMY AND REWARD STATE IS LOCAL ONLY"
  ]
  where
    guideRow y key description = Translate 0 y $ Pictures
      [ Translate (-260) 0 $
          drawSoul3DText 1.0 Theme.cTextGold Theme.cDarkBorder key
      , Translate 20 0 $
          drawTextShadow 0.13 Theme.cTextWhite Theme.cDarkBorder description
      ]
