module Views.MainMenu where

import Graphics.Gloss
import Assets
import Types
import Components.UI
import qualified Theme

drawMainMenu :: World -> Picture
drawMainMenu w = Pictures
  [ menuBackground w
  , Color (makeColor 0.005 0.015 0.04 0.68) $
      rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawAssetSlot w SlotMenuLogo
  , Translate 0 220 $
      drawSoul3DText 3.2 Theme.cTextGold Theme.cDarkBorder "DUNGEON SOUL"
  , drawResources w
  , actionButton w (-420, 155) "START" True
  , actionButton w (-420, 85) "CHARACTER SELECT" False
  , actionButton w (-420, 15) "SHOP" False
  , actionButton w (-420, -55) "UPGRADE STATS" False
  , actionButton w (-420, -125) "SETTINGS" False
  , actionButton w (-420, -195) "EXIT" False
  , Translate 590 305 $ drawSoulButton 76 40 "SET"
      (insideRect (mousePos w) (590, 305) 76 40)
  , drawAssetSlot w SlotMenuShopkeeper
  , Translate 350 (-210) $ drawPanel 500 150
  , Translate 350 (-180) $
      drawSoul3DText 1.4 Theme.cTextGold Theme.cDarkBorder "DUNGEON SOUL"
  , Translate 350 (-215) $
      drawTextShadow 0.13 Theme.cTextWhite Theme.cDarkBorder
        "Explore ancient halls, gather relics, and survive."
  , Translate 350 (-245) $
      drawTextShadow 0.12 Theme.cTextMuted Theme.cDarkBorder
        "LOCAL FRONTEND - CO-OP LOBBY USES MOCK PLAYERS"
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
drawResources w = Pictures
  [ Translate 460 305 $ Pictures
      [ drawPanel 230 90
      , Translate (-75) 0 $
          drawSoul3DText 1.25 Theme.cTextGold Theme.cDarkBorder
            (show (goldCoins w) ++ " GOLD")
      , Translate 85 (-35) $
          drawSoul3DText 1.25 Theme.cTextWhite Theme.cDarkBorder
            (show (gems w) ++ " GEMS")
      ]
  , drawAssetSlot w SlotMenuCoin
  , drawAssetSlot w SlotMenuGem
  , drawAssetSlot w SlotMenuSettings
  ]
