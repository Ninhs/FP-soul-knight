module Views.MapSelect where

import Graphics.Gloss
import Assets
import Types
import Components.UI
  ( drawAssetAt
  , mapAsset
  , menuBackground
  , drawSoul3DText
  , drawSoulButton
  , drawTextShadow
  , drawPanel
  , insideRect
  )
import qualified Theme

-- Chỉ có một map duy nhất trong game.
onlyMap :: MapSize
onlyMap = SmallMap

drawMapSelect :: World -> Picture
drawMapSelect w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , Translate 0 300 $
      drawSoul3DText 2.5 Theme.cTextGold Theme.cDarkBorder "SELECT A DUNGEON"
  , Translate (-300) 75 $ drawMapCard w
  , drawMapInfo
  , Translate (-405) (-285) $ drawSoulButton 210 52 "BACK"
      (insideRect (mousePos w) (-405, -285) 210 52)
  , Translate 40 (-285) $ drawSoulButton 180 52 "SOLO"
      (insideRect (mousePos w) (40, -285) 180 52)
  , Translate 330 (-285) $ drawSoulButton 180 52
      "CO-OP MOCK"
      (insideRect (mousePos w) (330, -285) 180 52)
  ]

drawMapCard :: World -> Picture
drawMapCard w = Pictures
  [ drawPanel 500 390
  , Translate 0 55 $
      drawAssetAt w (mapAsset onlyMap) (0, 0) (440, 280) Contain
  , Translate 0 (-95) $
      drawSoul3DText 1.3 Theme.cTextWhite Theme.cDarkBorder (mapName onlyMap)
  , Translate 0 (-140) $
      drawSoul3DText 1.0 Theme.cAccentGreen Theme.cDarkBorder "UNLOCKED"
  ]

drawMapInfo :: Picture
drawMapInfo = Translate 280 75 $ Pictures
  [ drawPanel 500 390
  , Translate 0 145 $
      drawSoul3DText 1.7 Theme.cTextGold Theme.cDarkBorder "MAP DETAILS"
  , infoLine 75 "BIOME" (mapName onlyMap)
  , infoLine 20 "DIFFICULTY" (mapDifficulty onlyMap)
  , infoLine (-35) "RECOMMENDED" (mapRecommended onlyMap)
  , infoLine (-90) "MODE" "LOCAL PREVIEW"
  , Translate 0 (-155) $
      drawTextShadow 0.11 Theme.cTextMuted Theme.cDarkBorder
        "Artwork only - no collision data."
  ]
  where
    infoLine y label value = Translate 0 y $ Pictures
      [ Translate (-165) 0 $
          drawSoul3DText 1.0 Theme.cTextMuted Theme.cDarkBorder label
      , Translate 80 0 $
          drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder value
      ]

mapName :: MapSize -> String
mapName _ = "ANCIENT FOREST"

mapDifficulty :: MapSize -> String
mapDifficulty _ = "EASY"

mapRecommended :: MapSize -> String
mapRecommended _ = "LEVEL 1"