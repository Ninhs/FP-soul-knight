module Views.MainMenu where

import Graphics.Gloss
import Assets
import Types
import Components.UI
import qualified Theme

drawMainMenu :: World -> Picture
drawMainMenu w = Pictures
  [ menuBackground w
  , Color (makeColor 0 0 0 0) $
      rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawAssetSlot w SlotMenuLogo
  , drawResources w
  , actionButton w (-420, 155) "START" True
  , actionButton w (-420, 85) "CHARACTER SELECT" False
  , actionButton w (-420, 15) "SHOP" False
  , actionButton w (-420, -55) "UPGRADE STATS" False
  , actionButton w (-420, -125) "HOW TO PLAY" False
  , actionButton w (-420, -195) "EXIT" False
  , Translate 590 305 $ drawSoulButton 76 40 "SET"
      (insideRect (mousePos w) (590, 305) 76 40)
  ]

actionButton :: World -> (Float, Float) -> String -> Bool -> Picture
actionButton w position label isPrimary
  | isPrimary = Translate x y $
      drawSoulButton 300 54 label hovered
  | otherwise =
      drawButton position 300 54 label hovered False False
  where
    (x, y) = position
    hovered = insideRect (mousePos w) position 300 54

drawResources :: World -> Picture
drawResources w =
  Translate 410 300 $ Scale s s $ Translate (-410) (-300) $ Pictures
    [ Translate 410 300 $ drawPanel 280 112
    , Translate 340 329 $
        drawSoul3DText 1.25 Theme.cTextGold Theme.cDarkBorder
          (show (goldCoins w) ++ " GOLD")
    , Translate 340 274 $
        drawSoul3DText 1.25 Theme.cTextWhite Theme.cDarkBorder
          (show (gems w) ++ " GEMS")
    , drawAssetSlot w SlotMenuCoin
    , drawAssetSlot w SlotMenuGem
    ]
  where
    s = 0.75

-- Tiêu đề đậm hơn (viền nhiều lớp) và căn giữa theo chiều ngang.
-- y: vị trí dọc (0 = giữa khung hình 720 px)
drawTitle :: Float -> Color -> Color -> Float -> String -> Picture
drawTitle size textColor borderColor y label =
  Translate x y $ Scale k k $ Pictures $
    [ Translate (dx * 6) (dy * 6) $ Color borderColor $ Text label
    | (dx, dy) <- outline
    ]
    ++ [ Color textColor $ Text label ]
  where
    k = size * 0.12
    charAdvance = 60        -- độ rộng ước lượng một ký tự; chỉnh nếu chữ lệch
    x = -(fromIntegral (length label) * charAdvance * k) / 2
    outline = [ (i, j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i, j) /= (0, 0) ]