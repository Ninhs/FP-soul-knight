module Components.UI where

import Graphics.Gloss
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Assets
import Types (ClientState(..), World, Character(..), MapSize(..))
import qualified Theme as Theme

cDarkBorder, cPrimaryGold, cHoverGold, cTextWhite, cAccentRed :: Color
cDarkBorder  = Theme.cDarkBorder
cPrimaryGold = Theme.cPrimaryGold
cHoverGold   = Theme.cHoverGold
cTextWhite   = Theme.cTextWhite
cAccentRed   = Theme.cAccentRed

drawSoul3DText :: Float -> Color -> Color -> String -> Picture
drawSoul3DText scaleFactor textColor borderColor label =
  Scale (scaleFactor * 0.12) (scaleFactor * 0.12) $ Pictures
    [ Translate 4 (-4) $ Color borderColor $ Text label
    , Translate 3 (-3) $ Color borderColor $ Text label
    , Translate 2 (-2) $ Color borderColor $ Text label
    , Translate 1 (-1) $ Color borderColor $ Text label
    , Color textColor $ Text label
    ]

drawSoulButton :: Float -> Float -> String -> Bool -> Picture
drawSoulButton width height label isHovered = Pictures
  [ Scale (width / 96) (height / 36) $
      if isHovered then uiButtonHover else uiButtonNormal
  , Translate textOffsetX (-7) $
      drawSoul3DText labelScale cTextWhite cDarkBorder label
  ]
  where
    labelWidth = fromIntegral (length label) * 11.2
    labelScale = min 2.2 (max 0.8 ((width - 18) / max 1 labelWidth))
    textOffsetX = -(labelWidth * labelScale / 2)

-- The source pack is PNG, while this project currently uses Gloss loadBMP.
-- Converted BMP copies are kept in client/assets/ui and assets/ui.
loadUiSprite :: FilePath -> Picture
loadUiSprite filename = unsafePerformIO $ do
  let candidates = ["client" </> "assets" </> "ui" </> filename,
                   "assets" </> "ui" </> filename]
  existing <- firstExisting candidates
  case existing of
    Nothing -> pure Blank
    Just path -> loadBMP path
  where
    firstExisting [] = pure Nothing
    firstExisting (path : rest) = do
      exists <- doesFileExist path
      if exists then pure (Just path) else firstExisting rest
{-# NOINLINE loadUiSprite #-}

uiButtonNormal, uiButtonHover, uiButtonPressed, uiButtonDisabled :: Picture
uiButtonNormal = loadUiSprite "button_medium_normal.bmp"
uiButtonHover = loadUiSprite "button_medium_hover.bmp"
uiButtonPressed = loadUiSprite "button_medium_pressed.bmp"
uiButtonDisabled = loadUiSprite "button_medium_disabled.bmp"

drawButton
  :: (Float, Float)
  -> Float
  -> Float
  -> String
  -> Bool
  -> Bool
  -> Bool
  -> Picture
drawButton (x, y) width height label isHovered isSelected isDisabled =
  Translate x y $ Pictures
    [ Scale (width / 96) (height / 36) $ buttonSprite
    , drawButtonLabel width label
    ]
  where
    buttonSprite
      | isDisabled = uiButtonDisabled
      | isSelected = uiButtonPressed
      | isHovered = uiButtonHover
      | otherwise = uiButtonNormal

drawButtonLabel :: Float -> String -> Picture
drawButtonLabel width label = Pictures
  [ Translate (-(fromIntegral (length labelLine) * 11.2 * labelScale / 2))
      (lineOffset * labelScale / 1.7) $
      drawSoul3DText labelScale cTextWhite cDarkBorder labelLine
  | (labelLine, lineOffset) <- zip (lines label) offsets
  ]
  where
    lineCount = length (lines label)
    longestLine = maximum (1 : map length (lines label))
    labelWidth = fromIntegral longestLine * 11.2
    labelScale = min 1.7 (max 0.8 ((width - 18) / labelWidth))
    offsets = [fromIntegral (lineCount - index - 1) * 9
      | index <- [0 .. lineCount - 1]]

drawTextShadow :: Float -> Color -> Color -> String -> Picture
drawTextShadow scaleFactor textColor shadowColor label =
  Scale scaleFactor scaleFactor $ Pictures
    [ Translate 2 (-2) $ Color shadowColor $ Text label
    , Color textColor $ Text label
    ]

drawPanel :: Float -> Float -> Picture
drawPanel = drawPanelWithBorder Theme.cPanelBorder

drawPanelWithBorder :: Color -> Float -> Float -> Picture
drawPanelWithBorder borderColor width height = Pictures
  [ Color borderColor $ rectangleSolid width height
  , Color Theme.cPanelBg $
      rectangleSolid (max 0 (width - 6)) (max 0 (height - 6))
  ]

drawHealthBar, drawEnergyBar, drawArmorBar
  :: Float -> Float -> Float -> Float -> Picture
drawHealthBar = drawMeter Theme.cHealthRed
drawEnergyBar = drawMeter Theme.cAccentBlue
drawArmorBar = drawMeter Theme.cArmorBlue

drawProgressBar :: (Float, Float) -> Float -> Float -> Float -> Color -> String -> Picture
drawProgressBar (x, y) width height ratio fillColor label =
  Translate x y $ Pictures
    [ Color Theme.cDarkBorder $ rectangleSolid width height
    , Color (makeColor 0.025 0.035 0.055 1.0) $
        rectangleSolid (max 0 (width - 4)) (max 0 (height - 4))
    , Translate (-(width - 4) / 2 + fillWidth / 2) 0 $
        Color fillColor $ rectangleSolid fillWidth (max 0 (height - 6))
    , if null label
        then Blank
        else Translate (width / 2 + 8) (-5) $
          drawTextShadow 0.12 Theme.cTextWhite Theme.cDarkBorder label
    ]
  where
    fillWidth = max 0 (width - 6) * max 0 (min 1 ratio)

drawMeter :: Color -> Float -> Float -> Float -> Float -> Picture
drawMeter fillColor width height current maxValue = Pictures
  [ Color Theme.cDarkBorder $ rectangleSolid width height
  , Color (makeColor 0.025 0.035 0.055 1.0) $
      rectangleSolid (max 0 (width - 4)) (max 0 (height - 4))
  , Translate (-(width - 4) / 2 + fillWidth / 2) 0 $
      Color fillColor $ rectangleSolid fillWidth (max 0 (height - 6))
  ]
  where
    ratio
      | maxValue <= 0 = 0
      | otherwise = max 0 (min 1 (current / maxValue))
    fillWidth = max 0 (width - 6) * ratio

drawCoinCounter :: Int -> Picture
drawCoinCounter coins =
  drawSoul3DText 1.5 Theme.cTextGold Theme.cDarkBorder
    (show (max 0 coins) ++ " G")

drawTooltip :: Float -> Float -> String -> Picture
drawTooltip width height message = Pictures
  [ drawPanel width height
  , Translate (-(fromIntegral (length message) * 3.5)) (-4) $
      drawTextShadow 0.14 Theme.cTextWhite Theme.cDarkBorder message
  ]

drawPopup :: Float -> Float -> String -> Picture
drawPopup width height message = Pictures
  [ Color Theme.cPanelGlow $ rectangleSolid width height
  , Color Theme.cPanelBg $
      rectangleSolid (max 0 (width - 6)) (max 0 (height - 6))
  , Translate (-(fromIntegral (length message) * 4)) (-5) $
      drawSoul3DText 1.4 Theme.cTextWhite Theme.cDarkBorder message
  ]

drawBackButton :: Bool -> Picture
drawBackButton = drawSoulButton 180 Theme.buttonHeight "BACK"

drawInfoPanel :: Float -> Float -> String -> String -> Picture
drawInfoPanel width height title body = Pictures
  [ drawPanel width height
  , Translate (-(fromIntegral (length title) * 5)) (height * 0.22) $
      drawSoul3DText 1.5 Theme.cTextGold Theme.cDarkBorder title
  , Translate (-(fromIntegral (length body) * 3.5)) (-height * 0.12) $
      drawTextShadow 0.14 Theme.cTextWhite Theme.cDarkBorder body
  ]

drawPauseOverlay :: (Float, Float) -> Picture
drawPauseOverlay mouse = Pictures
  [ Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawPanel 420 280
  , Translate 0 80 $
      drawSoul3DText 2.8 Theme.cTextGold Theme.cDarkBorder "PAUSED"
  , Translate 0 20 $
      drawSoulButton 240 52 "RESUME" (insideRect mouse (0, 20) 240 52)
  , Translate 0 (-50) $
      drawSoulButton 240 52 "MAIN MENU" (insideRect mouse (0, -50) 240 52)
  ]

insideRect :: (Float, Float) -> (Float, Float) -> Float -> Float -> Bool
insideRect (mx, my) (rx, ry) width height =
  width >= 0 && height >= 0
    && mx >= (rx - width / 2) && mx <= (rx + width / 2)
    && my >= (ry - height / 2) && my <= (ry + height / 2)

-- Hàm vẽ nút bấm bằng Asset
drawAssetButton :: Picture -> Picture -> String -> Bool -> Picture
drawAssetButton normalPic hoverPic label isHover =
  case if isHover then hoverPic else normalPic of
    Blank -> drawSoulButton 240 60 label isHover
    buttonPic -> Pictures
      [ Scale 2.5 1.65 buttonPic
      , Translate 0 (-10) $
          drawSoul3DText 1.6 (makeColor 1 1 1 1) (makeColor 0 0 0 1) label
      ]

menuBackground :: World -> Picture
menuBackground world =
  case backgroundImage (appAssets world) of
    Blank -> Pictures
      [ Color (makeColor 0.035 0.055 0.10 1) $
          rectangleSolid Theme.screenWidth Theme.screenHeight
      , Color (makeColor 0.08 0.14 0.20 1) $
          Translate 0 (-190) $ rectangleSolid Theme.screenWidth 340
      ]
    image -> image

drawAssetAt
  :: World
  -> AssetId
  -> (Float, Float)
  -> (Float, Float)
  -> FitMode
  -> Picture
drawAssetAt world asset (x, y) (width, height) fit =
  Translate x y $ case assetPicture of
    Blank -> drawAssetFallback asset width height
    image -> drawFitted image sourceSize (width, height) fit
  where
    assetPicture = assetImage (appAssets world) asset
    sourceSize = assetDimensions asset

drawAssetSlot :: World -> AssetSlot -> Picture
drawAssetSlot world slot =
  Translate x y $ Pictures
    [ case hotbarSlotImage (appAssets world) of
        Blank -> Pictures
          [ Color Theme.cDarkBorder $ rectangleSolid 52 52
          , Color Theme.cPanelBg $ rectangleSolid 46 46
          ]
        slotPicture -> Scale (52 / 44) (52 / 44) slotPicture
    , drawAssetAt world asset (0, 0) (32, 32) Contain
    ]
  where
    (x, y, asset) = case slot of
      SlotHudArmor -> (-520, 279, AssetArmor)
      SlotHudAvatar -> (-600, 334, characterAsset (selectedChar world))
      SlotHudCoin -> (300, 320, AssetCoin)
      SlotHudHealth -> (-520, 304, AssetHealth)
      SlotHudSkill1 -> (390, -285, AssetSkill)
      SlotHudSkill2 -> (490, -285, AssetSkill)
      SlotMenuCoin -> (300, 329, AssetCoin)
      SlotMenuGem -> (300, 274, AssetGem)
      SlotMenuLogo -> (0, 275, AssetSwordsman)
      SlotMenuSettings -> (590, 305, AssetSettings)
      SlotMenuShopkeeper -> (560, -150, AssetDemon)
      SlotRewardChest -> (0, 95, AssetChest)
      SlotShopkeeper -> (-500, 35, AssetDemon)

characterAsset :: Character -> AssetId
characterAsset Swordsman = AssetSwordsman
characterAsset Archer = AssetArcher
characterAsset Knight = AssetSwordsman
characterAsset Bomber = AssetArcher
characterAsset Mage = AssetArcher

mapAsset :: MapSize -> AssetId
mapAsset _ = AssetDungeonMap

assetImage :: GameAssets -> AssetId -> Picture
assetImage assets asset = case asset of
  AssetBackground -> backgroundImage assets
  AssetDungeonMap -> dungeonMapImage assets
  AssetSwordsman -> firstFrame (soldierIdle assets)
  AssetArcher -> firstFrame (archerIdle assets)
  AssetKnight -> firstFrame (soldierIdle assets)
  AssetBomber -> firstFrame (archerIdle assets)
  AssetMage -> firstFrame (archerIdle assets)
  AssetMonster -> firstFrame (monsterIdle assets)
  AssetOrc -> firstFrame (orcIdle assets)
  AssetDemon -> firstFrame (demonIdle assets)
  AssetGem -> Blank
  AssetSettings -> Blank
  AssetBoss -> firstFrame (demonIdle assets)
  AssetAttackEffect -> Blank
  AssetProjectile -> Blank
  AssetPortal -> Blank
  AssetChest -> goldChestImage assets
  AssetCoin -> coinBagImage assets
  AssetHealth -> Blank
  AssetArmor -> blueShieldImage assets
  AssetDamageRune -> Blank
  AssetHealthPotion -> Blank
  AssetHeavyArmor -> Blank
  AssetLargePotion -> Blank
  AssetLightArmor -> Blank
  AssetMinimapFrame -> Blank
  AssetSkill -> Blank
  AssetSwiftBoots -> Blank
  AssetAvatarFrame -> Blank
  where
    firstFrame [] = Blank
    firstFrame (frame : _) = frame

assetDimensions :: AssetId -> (Float, Float)
assetDimensions AssetBackground = (1280, 720)
assetDimensions AssetDungeonMap = (2752, 1536)
assetDimensions _ = (32, 32)

drawFitted :: Picture -> (Float, Float) -> (Float, Float) -> FitMode -> Picture
drawFitted Blank _ (width, height) _ =
  Color (makeColor 0.20 0.36 0.50 1) $ rectangleSolid width height
drawFitted image (sourceWidth, sourceHeight) (width, height) fit =
  Scale scaleX scaleY image
  where
    safeWidth = max 1 sourceWidth
    safeHeight = max 1 sourceHeight
    containScale = min (width / safeWidth) (height / safeHeight)
    coverScale = max (width / safeWidth) (height / safeHeight)
    (scaleX, scaleY) = case fit of
      Contain -> (containScale, containScale)
      Cover -> (coverScale, coverScale)
      Stretch -> (width / safeWidth, height / safeHeight)

drawAssetFallback :: AssetId -> Float -> Float -> Picture
drawAssetFallback asset width height =
  Scale (width / 56) (height / 56) $ case asset of
    AssetAttackEffect -> Color gold $ thickCircle 24 5
    AssetPortal -> Pictures
      [ Color blue $ thickCircle 25 6
      , Color paleBlue $ thickCircle 14 3
      ]
    AssetProjectile -> Color orange $ circleSolid 10
    AssetCoin -> Pictures
      [ Color gold $ circleSolid 18
      , Color darkGold $ circle 11
      ]
    AssetGem -> Color paleBlue $
      Polygon [(-18, 8), (-9, 18), (9, 18), (18, 8), (12, -14), (0, -21), (-12, -14)]
    AssetSettings -> Pictures
      [ Color paleBlue $ thickCircle 15 7
      , Color Theme.cDarkBorder $ circleSolid 5
      ]
    AssetHealth -> healthMark
    AssetHealthPotion -> healthMark
    AssetLargePotion -> healthMark
    AssetArmor -> armorMark
    AssetLightArmor -> armorMark
    AssetHeavyArmor -> armorMark
    AssetChest -> Pictures
      [ Color brown $ rectangleSolid 38 30
      , Color gold $ rectangleSolid 8 30
      , Translate 0 5 $ Color paleGold $ rectangleSolid 8 5
      ]
    AssetBoss -> Color red $ thickCircle 21 5
    AssetDemon -> Color purple $ circleSolid 19
    AssetMinimapFrame -> Color blue $ rectangleSolid 50 38
    AssetSkill -> Color purple $ Polygon [(-18, 0), (0, 18), (18, 0), (0, -18)]
    AssetDamageRune -> Color orange $ Polygon [(-18, -15), (0, 20), (18, -15)]
    AssetSwiftBoots -> Color paleBlue $ Polygon [(-18, 15), (5, 15), (18, 0), (5, -15), (-18, -15)]
    AssetAvatarFrame -> Color gold $ thickCircle 23 4
    AssetDungeonMap -> Color (makeColor 0.18 0.25 0.25 1) $ rectangleSolid 56 56
    AssetBackground -> Color (makeColor 0.05 0.08 0.15 1) $ rectangleSolid 56 56
    _ -> Color blue $ rectangleSolid 36 36
  where
    gold = makeColor 1.0 0.75 0.12 1
    paleGold = makeColor 1.0 0.91 0.48 1
    darkGold = makeColor 0.6 0.34 0.06 1
    blue = makeColor 0.18 0.58 0.95 1
    paleBlue = makeColor 0.52 0.85 1.0 1
    orange = makeColor 1.0 0.39 0.14 1
    red = makeColor 0.9 0.12 0.18 1
    purple = makeColor 0.66 0.36 0.95 1
    brown = makeColor 0.39 0.22 0.10 1
    healthMark = Pictures
      [ Color red $ rectangleSolid 30 10
      , Color red $ rectangleSolid 10 30
      ]
    armorMark = Color blue $ Polygon
      [(-18, 12), (0, 21), (18, 12), (14, -8), (0, -20), (-14, -8)]