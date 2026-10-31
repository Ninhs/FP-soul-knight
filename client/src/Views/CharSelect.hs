module Views.CharSelect where

import Graphics.Gloss
import Assets
import Types
import Components.UI
import qualified Theme

drawCharSelect :: World -> Picture
drawCharSelect w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , Translate 0 300 $
      drawSoul3DText 2.5 Theme.cTextGold Theme.cDarkBorder "CHARACTER SELECT"
  , Translate (-300) 75 $ drawPanel 370 430
  , drawSelectedCharacter w
  , drawCharacterInfo w
  , drawButton (-300, -190) 180 48 "SWORDSMAN"
      (insideRect (mousePos w) (-300, -190) 180 48)
      (selectedChar w == Swordsman) False
  , drawButton (-95, -190) 160 48 "ARCHER"
      (insideRect (mousePos w) (-95, -190) 160 48)
      (selectedChar w == Archer) False
  , Translate (-220) (-280) $ drawSoulButton 190 52 "CLOSE"
      (insideRect (mousePos w) (-220, -280) 190 52)
  , Translate (220) (-280) $ drawSoulButton 220 52 "SELECT"
      (insideRect (mousePos w) (220, -280) 220 52)
  ]

drawSelectedCharacter :: World -> Picture
drawSelectedCharacter w = Pictures
  [ drawAssetAt w (characterAsset (selectedChar w)) (-300, 105) (230, 210) Contain
  , characterSlot w Swordsman (-375)
  , characterSlot w Archer (-225)
  ]

characterSlot :: World -> Character -> Float -> Picture
characterSlot w character x = Translate x (-75) $ Pictures
  [ Color (if selectedChar w == character then Theme.cPrimaryGold else Theme.cPanelBorder)
      $ rectangleSolid 104 104
  , Color Theme.cPanelBg $ rectangleSolid 98 98
  , drawAssetAt w (characterAsset character) (0, 8) (64, 64) Contain
  , Translate 0 (-36) $
      drawTextShadow 0.10 Theme.cTextWhite Theme.cDarkBorder (characterLabel character)
  ]

characterLabel :: Character -> String
characterLabel Swordsman = "SWORDSMAN"
characterLabel Archer = "ARCHER"
characterLabel _ = "HERO"

drawCharacterInfo :: World -> Picture
drawCharacterInfo w = Translate 220 65 $ Pictures
  [ drawPanel 510 410
  , drawTitleAt 0 2.0 Theme.cTextGold Theme.cDarkBorder 145 name
  , drawCenteredText 0 0.15 Theme.cTextWhite Theme.cDarkBorder 100 description
  , drawTitleAt 0 1.1 Theme.cTextMuted Theme.cDarkBorder 40 ("WEAPON: " ++ weapon)
  , statLine 0 (-30) "HP" (characterMaxHp stats) 150 Theme.cHealthRed
  , statLine 0 (-82) "ATK" (characterDamage stats) 36 Theme.cPrimaryGold
  , statLine 0 (-134) "SPD" (characterMoveSpeed stats) 320 Theme.cAccentGreen
  ]
  where
    stats = characterStats (selectedChar w)
    (name, description, weapon) = heroDetails (selectedChar w)
    statLine x y label value maximumValue color = Translate x y $ Pictures
      [ Translate (-190) 0 $
          drawSoul3DText 1.0 Theme.cTextWhite Theme.cDarkBorder label
      , Translate (-70) 0 $
          drawProgressBar (0, 0) 190 12 (value / maximumValue) color ""
      , Translate 130 (-5) $
          drawTextShadow 0.12 Theme.cTextGold Theme.cDarkBorder
            (show (round value :: Int))
      ]

heroDetails :: Character -> (String, String, String)
heroDetails Swordsman = ("SWORDSMAN", "Balanced melee fighter.", "SWORD")
heroDetails Archer = ("ARCHER", "A quick ranged specialist.", "BOW")
heroDetails character = (show character, "A hero asset slot is reserved.", "TBD")
