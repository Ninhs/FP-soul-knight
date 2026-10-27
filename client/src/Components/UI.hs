module Components.UI where

import Graphics.Gloss
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
  [ Translate 3 (-3) $ Color cDarkBorder $
      rectangleSolid (width + 6) (height + 6)
  , Color cDarkBorder $ rectangleSolid (width + 4) (height + 4)
  , Color mainColor $ rectangleSolid width height
  , Translate (-2) 2 $ Color (makeColor 1.0 1.0 1.0 0.28) $
      rectangleSolid (max 0 (width - 4)) (max 0 (height - 4))
  , Color (if isHovered then Theme.cPanelGlow else mainColor) $
      rectangleSolid (max 0 (width - 8)) (max 0 (height - 8))
  , Translate textOffsetX (-7) $
      drawSoul3DText 2.2 cTextWhite cDarkBorder label
  ]
  where
    mainColor = if isHovered then cHoverGold else cPrimaryGold
    textOffsetX = -(fromIntegral (length label) * 10.5)

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
    [ Translate 3 (-3) $ Color cDarkBorder $
        rectangleSolid (width + 6) (height + 6)
    , Color buttonColor $ rectangleSolid width height
    , Color (makeColor 1 1 1 0.22) $
        rectangleSolid (max 0 (width - 6)) (max 0 (height - 6))
    , Color buttonColor $
        rectangleSolid (max 0 (width - 10)) (max 0 (height - 10))
    , drawButtonLabel label
    ]
  where
    buttonColor
      | isDisabled = Theme.cTextMuted
      | isSelected = Theme.cPrimaryGold
      | isHovered = Theme.cPanelGlow
      | otherwise = Theme.cAccentBlue

drawButtonLabel :: String -> Picture
drawButtonLabel label = Pictures
  [ Translate (-(fromIntegral (length labelLine) * 5.5)) lineOffset $
      drawSoul3DText 1.7 cTextWhite cDarkBorder labelLine
  | (labelLine, lineOffset) <- zip (lines label) offsets
  ]
  where
    lineCount = length (lines label)
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
      [ buttonPic
      , Translate 0 (-10) $
          drawSoul3DText 1.6 (makeColor 1 1 1 1) (makeColor 0 0 0 1) label
      ]