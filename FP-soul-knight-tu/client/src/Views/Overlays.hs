module Views.Overlays where

import Graphics.Gloss
import Types
import Components.UI
import qualified Theme

drawGameResult :: Bool -> World -> Picture
drawGameResult isVictory w = Pictures
  [ Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawPanel 560 410
  , Translate 0 125 $ drawSoul3DText 3.4 titleColor cDarkBorder title
  , Translate 0 40 $
      drawSoul3DText 1.6 cTextWhite cDarkBorder
        ("HERO LEVEL: " ++ show (playerLevel w))
  , Translate 0 0 $
      drawSoul3DText 1.5 cPrimaryGold cDarkBorder
        ("GOLD: " ++ show (goldCoins w) ++ " G")
  , Translate 0 (-42) $
      drawSoul3DText 1.5 Theme.cAccentBlue cDarkBorder
        ("EXP: " ++ show (playerExperience w))
  , if isVictory
      then Translate 0 (-82) $
        drawSoul3DText 1.2 Theme.cAccentGreen cDarkBorder
          ("RUN REWARD: +" ++ show (mapRewardGold (selectedMapSize w)) ++ " G")
      else Blank
  , Translate (-145) (-130) $
      drawSoulButton 250 58 "PLAY AGAIN"
        (insideRect mouse (-145, -130) 250 58)
  , Translate 155 (-130) $
      drawSoulButton 210 58 "MAIN MENU"
        (insideRect mouse (155, -130) 210 58)
  ]
  where
    mouse = mousePos w
    title = if isVictory then "VICTORY!" else "GAME OVER"
    titleColor = if isVictory then Theme.cAccentGreen else cAccentRed

drawShopNotice :: String -> (Float, Float) -> Picture
drawShopNotice message mouse = Pictures
  [ Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawInfoPanel 480 220 "NOTICE" message
  , Translate 0 (-70) $
      drawSoulButton 180 48 "OK" (insideRect mouse (0, -70) 180 48)
  ]

drawConfirmMenu :: (Float, Float) -> Picture
drawConfirmMenu mouse = Pictures
  [ Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawInfoPanel 500 230 "RETURN TO MENU?" "Your current run will be abandoned."
  , Translate (-125) (-72) $
      drawSoulButton 190 48 "STAY" (insideRect mouse (-125, -72) 190 48)
  , Translate 125 (-72) $
      drawSoulButton 190 48 "LEAVE" (insideRect mouse (125, -72) 190 48)
  ]
