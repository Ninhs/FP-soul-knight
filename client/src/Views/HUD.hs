module Views.HUD
  ( drawChestRewardOverlay
  , drawGameHUD
  , drawTutorialGuide
  ) where
import MapLayout (Link(..), Rect(..), RoomId(..), linkDoor, openLinks, permanentWalls, roomAt, roomRect)
import Graphics.Gloss
import Animation (AnimState, currentFrame, animKind, AnimKind(..), SpriteSet(..))
import Assets
import qualified MapLayout as Map
import qualified Types.GameState as GS
import Sprites (framesOf, playerSet)
import Types
import Components.UI
  ( characterAsset
  , drawAssetAt
  , drawAssetSlot
  , mapAsset
  , menuBackground
  , drawSoul3DText
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
           fromIntegral mapRows * Theme.dungeonTileSize) Stretch
      , drawDrops w 
      , drawWalls w
      , drawClosedDoors w
      , drawChest w
      , Pictures (map (drawEnemy w) (enemyCorpses w ++ enemies w))
      , Pictures (map (drawProjectile w) (projectiles w))
      , drawPlayer w
      , drawAttackEffect w (attackEffectTimer w) (attackEffectPos w)
      ]
  , drawFixedHud w
  , if tutorialVisible w then drawTutorialOverlay w else Blank
  ]
  where
    (px, py) = playerPos w
    (mapColumns, mapRows) = mapDimensions (selectedMapSize w)

drawWalls :: World -> Picture
drawWalls w =
  drawMapRects (makeColor 0.0 0.0 0.0 0) (Map.permanentWalls (selectedMapSize w))

drawClosedDoors :: World -> Picture
drawClosedDoors w =
  drawMapRects (makeColor 0.0 0.0 0.0 0)
    (Map.closedDoorRects (selectedMapSize w)
      (Map.openLinks (clearedRooms w)))

drawMapRects :: Color -> [Map.Rect] -> Picture
drawMapRects color = Pictures . map drawRect
  where
    drawRect bounds =
      Translate (Map.rectX bounds + Map.rectWidth bounds / 2)
        (Map.rectY bounds + Map.rectHeight bounds / 2)
        $ Color color
        $ rectangleSolid (Map.rectWidth bounds) (Map.rectHeight bounds)

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
  | not (enemyIsAlive enemy) = Translate x y $
      drawAnimated (enemyAnim enemy) (enemySprite enemy)
        (enemyFacingRight enemy) (0, 0) spriteSize (appAssets w)
  | otherwise = Translate x y $ Pictures
      [ drawAnimated (enemyAnim enemy) (enemySprite enemy)
          (enemyFacingRight enemy) (0, 0) spriteSize (appAssets w)
      , Translate 0 healthBarY $ enemyHealthBar enemy
      , Translate 0 (negate healthBarY) $
          drawTextShadow 0.10 Theme.cTextWhite Theme.cDarkBorder
            (enemyLabel (enemyKind enemy))
      ]
  where
    (x, y) = enemyPos enemy
    bossEnemy = enemyKind enemy == HighDamageEnemy
    spriteSize = if bossEnemy then (140, 140) else (54, 64)
    healthBarY = if bossEnemy then 78 else 42

-- Sprite frames use a 32 px source scale, so the boss is intentionally larger.
drawAnimated :: AnimState -> SpriteSet -> Bool -> (Float, Float) -> (Float, Float) -> GameAssets -> Picture
drawAnimated anim set facing (x, y) (width, height) assets =
  Translate x y $ Scale sx scale frame
  where
    frame = currentFrame (framesOf assets set (animKind anim)) anim
    scale = min (width / 32) (height / 32)
    sx = if facing then scale else negate scale

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
enemyLabel HighDamageEnemy = "BOSS"

drawPlayer :: World -> Picture
drawPlayer w = Pictures
  [ drawAnimated (playerAnim w) (playerSet (selectedChar w)) (playerFacingRight w)
      (playerPos w) (56, 68) (appAssets w)
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
drawAttackEffect _ duration position
  | duration <= 0 = Blank
  | otherwise = Translate x y $ Pictures
      [ Color (makeColor 1.0 0.78 0.18 0.9) $ thickCircle 34 5
      , Color (makeColor 1.0 0.92 0.55 0.85) $ circleSolid 7
      ]
  where
    (x, y) = position

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
  , Translate (-455) 236 $
      drawTextShadow 0.11 Theme.cAccentBlue Theme.cDarkBorder
        ("EXP " ++ show (playerExperience w) ++ " / " ++ show (playerLevel w * 100))
  , Translate 430 320 $ drawPanel 390 86
  , drawAssetSlot w SlotHudCoin
  , Translate 345 320 $ drawCoinCounter (goldCoins w)
  , Translate 345 291 $
      drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder
        "LOCAL DEMO"
  , drawMinimap w
  , Translate 560 320 $ drawSoulButton 130 42 "PAUSE"
      (insideRect (mousePos w) (560, 320) 130 42)
  , Translate 560 265 $ drawSoulButton 130 42 "SHOP"
      (insideRect (mousePos w) (560, 265) 130 42)
  , Translate 560 210 $ drawSoulButton 130 42 "UPGRADE"
      (insideRect (mousePos w) (560, 210) 130 42)
  , if inventoryVisible w then drawInventoryBar w else Blank
  , Translate 440 (-285) $
      drawTextShadow 0.10 Theme.cTextMuted Theme.cDarkBorder "SKILLS LOCKED"
  , Translate (-455) (-322) $
      drawTextShadow 0.13 Theme.cTextWhite Theme.cDarkBorder
        "WASD MOVE   /   LEFT CLICK AIM + ATTACK   /   E INTERACT"
  , if currentRoom w == Just Map.BossRoom && bossHealth w > 0
      then Translate 0 325 $
        drawBossHPBar "BOSS" (bossHealth w) (bossMaxHealth w)
      else Blank
  ]
rectToTuple :: Rect -> (Float, Float, Float, Float)
rectToTuple r = (rectX r, rectY r, rectWidth r, rectHeight r)

drawMinimap :: World -> Picture
drawMinimap w = Translate 390 165 $ Pictures
  [ drawPanel 190 140
  , drawAssetAt w AssetMinimapFrame (0, 0) (190, 140) Contain
  , Translate 0 1 $ Pictures $
      [ Color (makeColor 0.025 0.055 0.075 1) $ rectangleSolid radarW radarH ]
      ++ map drawRoomTint [StartRoom, NormalA, NormalB, BossRoom]
      ++ map (drawWall . rectToTuple) (permanentWalls size)
      ++ map drawClosedDoor closedLinks
      ++ map drawEnemyMarker (filter enemyIsAlive (enemies w))
      ++ [drawPlayerMarker]
  ]
  where
    radarW = 130
    radarH = 78
    size = selectedMapSize w
    (cols, rows) = mapDimensions size
    mapW = fromIntegral cols * Theme.dungeonTileSize
    mapH = fromIntegral rows * Theme.dungeonTileSize
    current = roomAt size (playerPos w)
    closedLinks =
      [ l | l <- [LinkBLtoTL, LinkTLtoTR, LinkTRtoBR]
          , l `notElem` openLinks (clearedRooms w) ]

    -- Pixel (gốc ở tâm map) -> tọa độ trong khung radar (gốc ở tâm khung)
    toRadar (x, y) = (x / mapW * radarW, y / mapH * radarH)

    -- Vẽ một hình chữ nhật (x, y, rộng, cao) của map lên radar
    rectPic (x, y, rw, rh) col =
      let (cx, cy) = toRadar (x + rw / 2, y + rh / 2)
          sw = rw / mapW * radarW
          sh = rh / mapH * radarH
      in Translate cx cy $ Color col $ rectangleSolid (max 1 sw) (max 1 sh)

    drawRoomTint r = rectPic (rectToTuple (roomRect size r)) (roomColor r)

    drawWall rect = rectPic rect (makeColor 0.75 0.75 0.75 1)

    drawClosedDoor l = rectPic (rectToTuple (linkDoor size l)) Theme.cAccentRed

    roomColor r
      | current == Just r = makeColor 0.25 0.55 0.65 1
      | r `elem` clearedRooms w = makeColor 0.15 0.4 0.22 1
      | r == BossRoom = makeColor 0.4 0.12 0.12 1
      | otherwise = makeColor 0.13 0.2 0.25 1

    drawPlayerMarker =
      let (x, y) = toRadar (playerPos w)
      in Translate x y $ Pictures
        [ Color Theme.cTextWhite $ circleSolid 5
        , Color Theme.cAccentBlue $ circleSolid 3
        ]

    drawEnemyMarker enemy =
      let (x, y) = toRadar (enemyPos enemy)
          isBoss = enemyKind enemy == HighDamageEnemy
          radius = if isBoss then 6 else 4
          col = if isBoss then Theme.cTextGold else Theme.cAccentRed
      in Translate x y $ Color col $ circleSolid radius

drawInventoryBar :: World -> Picture
drawInventoryBar w = Translate 25 (-255) $ Pictures
  [ drawPanel 360 74
  , Translate 0 25 $
      drawSoul3DText 0.9 Theme.cTextGold Theme.cDarkBorder "INVENTORY  [I]"
  , Pictures
      [ drawInventorySlot w index kind count x
      | (index, kind, x) <-
          [ (1, SmallHealthPotion, -132)
          , (2, LargeHealthPotion, 44)
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
  , guideRow 15 "SKILLS" "Not available in the local preview"
  , guideRow (-35) "E" "Interact with nearby objects"
  , guideRow (-85) "I / 1-4" "Show inventory / use an item"
  , Translate (-285) (-150) $
      drawTextShadow 0.11 Theme.cTextMuted Theme.cDarkBorder
        "Local preview data is temporary and is not saved."
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

drawDrops :: World -> Picture
drawDrops w = Pictures
  [ Translate (x * 35) (y * 35) $ Pictures
      [ Color black $ circleSolid 9
      , Color (dropColor (GS.dType d)) $ circleSolid 6
      ]
  | d <- GS.drops (localMapState w)
  , let (x, y) = GS.dPosition d
  ]
  where
    dropColor (GS.GoldDrop _) = yellow
    dropColor (GS.ExpDrop _)  = green
    dropColor (GS.HealDrop _) = rose