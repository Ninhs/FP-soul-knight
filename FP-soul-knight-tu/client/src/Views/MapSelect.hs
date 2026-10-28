module Views.MapSelect where

import Graphics.Gloss
import Assets
import Types
import Components.UI
  ( drawSoul3DText
  , drawSoulButton
  , drawTextShadow
  , drawPanel
  , insideRect
  )
import qualified Theme

drawMapSelect :: World -> Picture
drawMapSelect w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , Translate 0 300 $
      drawSoul3DText 2.5 Theme.cTextGold Theme.cDarkBorder "SELECT A DUNGEON"
  , Translate (-300) 75 $ drawMapCard w
  , drawMapInfo w
  , Translate (-405) (-285) $ drawSoulButton 210 52 "BACK"
      (insideRect (mousePos w) (-405, -285) 210 52)
  , Translate 40 (-285) $ drawSoulButton 180 52 "SOLO"
      (insideRect (mousePos w) (40, -285) 180 52)
  , Translate 330 (-285) $ drawSoulButton 180 52
      (if isMultiplayer w then "START CO-OP" else "CO-OP LOBBY")
      (insideRect (mousePos w) (330, -285) 180 52)
  ]

drawMapCard :: World -> Picture
drawMapCard w = Pictures
  [ drawPanel 500 390
  , Translate 0 55 $
      drawAssetAt w (mapAsset (selectedMapSize w)) (0, 0) (440, 280) Contain
  , Translate 0 (-95) $
      drawSoul3DText 1.3 Theme.cTextWhite Theme.cDarkBorder (mapName (selectedMapSize w))
  , Translate 0 (-140) $
      drawSoul3DText 1.0 Theme.cAccentGreen Theme.cDarkBorder "UNLOCKED"
  , Translate (-145) (-198) $ drawSoulButton 150 42 "< PREVIOUS"
      (insideRect (mousePos w) (-445, -123) 150 42)
  , Translate 145 (-198) $ drawSoulButton 150 42 "NEXT >"
      (insideRect (mousePos w) (-155, -123) 150 42)
  ]

drawMapInfo :: World -> Picture
drawMapInfo w = Translate 280 75 $ Pictures
  [ drawPanel 500 390
  , Translate 0 145 $
      drawSoul3DText 1.7 Theme.cTextGold Theme.cDarkBorder "MAP DETAILS"
  , infoLine 75 "BIOME" (mapName (selectedMapSize w))
  , infoLine 20 "DIFFICULTY" (mapDifficulty (selectedMapSize w))
  , infoLine (-35) "RECOMMENDED" (mapRecommended (selectedMapSize w))
  , infoLine (-90) "CLEAR REWARD"
      (show (mapRewardGold (selectedMapSize w)) ++ " GOLD")
  , Translate 0 (-155) $
      drawTextShadow 0.11 Theme.cTextMuted Theme.cDarkBorder
        "Map art is an optional image slot."
  ]
  where
    infoLine y label value = Translate 0 y $ Pictures
      [ Translate (-165) 0 $
          drawSoul3DText 1.0 Theme.cTextMuted Theme.cDarkBorder label
      , Translate 80 0 $
          drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder value
      ]

mapName :: MapSize -> String
mapName SmallMap = "ANCIENT FOREST"
mapName MediumMap = "CRYSTAL MINES"
mapName LargeMap = "FROST CITADEL"

mapDifficulty :: MapSize -> String
mapDifficulty SmallMap = "EASY"
mapDifficulty MediumMap = "NORMAL"
mapDifficulty LargeMap = "HARD"

mapRecommended :: MapSize -> String
mapRecommended SmallMap = "LEVEL 1"
mapRecommended MediumMap = "LEVEL 3"
mapRecommended LargeMap = "LEVEL 5"
