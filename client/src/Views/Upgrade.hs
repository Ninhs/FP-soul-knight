module Views.Upgrade
  ( drawUpgrade
  , upgradeHit
  , upgradePrice
  ) where

import Data.Char (toUpper)
import Graphics.Gloss
import Assets
import Types
import Components.UI
  ( drawAssetAt
  , menuBackground
  , drawSoul3DText
  , drawSoulButton
  , drawTextShadow
  , drawPanel
  , drawProgressBar
  , insideRect
  )
import qualified Theme

drawUpgrade :: World -> Picture
drawUpgrade w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawPanel 1120 680
  , Translate 0 302 $
      drawSoul3DText 2.4 Theme.cTextGold Theme.cDarkBorder
        ("UPGRADE  " ++ map toUpper (show (selectedChar w)))
  , Translate 0 262 $
      drawTextShadow 0.13 Theme.cTextWhite Theme.cDarkBorder
        "Upgrades apply to the selected hero only. Change hero in CHARACTER SELECT."
  , Translate (-340) 225 $
      drawSoul3DText 1.1 Theme.cTextGold Theme.cDarkBorder
        ("AVAILABLE GOLD: " ++ show (goldCoins w))
  , Translate 340 225 $
      drawSoul3DText 1.1 Theme.cTextWhite Theme.cDarkBorder
        ("GEMS: " ++ show (gems w))
  , Pictures
      [ upgradeCard w kind x y
      | (kind, x, y) <- upgradeChoices w
      ]
  , Translate (-300) (-270) $ drawSoulButton 210 48 "BACK"
      (insideRect (mousePos w) (-300, -270) 210 48)
  , Translate 100 (-270) $ drawSoulButton 250 48 "REFRESH 5 GEMS"
      (insideRect (mousePos w) (100, -270) 250 48)
  , Translate 410 (-270) $
      drawTextShadow 0.11 Theme.cTextMuted Theme.cDarkBorder
        ("REFRESHES: " ++ show (upgradeRefreshes w))
  ]

upgradeCard :: World -> UpgradeKind -> Float -> Float -> Picture
upgradeCard w kind x y =
  Translate x y $ Pictures
    [ drawPanel 440 205
    , drawAssetAt w (upgradeAsset kind) (-145, 55) (56, 56) Contain
    , Translate (-55) 70 $
        drawSoul3DText 1.2 Theme.cTextWhite Theme.cDarkBorder (upgradeName kind)
    , Translate (-55) 40 $
        drawTextShadow 0.13 Theme.cTextMuted Theme.cDarkBorder
          ("CURRENT: " ++ show (round value :: Int) ++ "  LV " ++ show level)
    , Translate 20 2 $
        drawProgressBar (0, 0) 170 14 (value / statLimit) Theme.cAccentGreen ""
    , Translate (-92) (-34) $
        drawSoul3DText 1.05 Theme.cTextGold Theme.cDarkBorder
          ("NEXT LEVEL COST: " ++ show price ++ " G")
    , Translate 120 (-72) $
        drawSoulButton 110 44 "+"
          (insideRect (mousePos w) (x + 120, y - 72) 110 44)
    , Translate (-105) (-72) $
        drawTextShadow 0.12 stateColor Theme.cDarkBorder stateLabel
    ]
  where
    (value, level) = case kind of
      MaxHealthUpgrade -> (playerMaxHealth w, healthUpgradeLevel w)
      DamageUpgrade -> (playerDamage w, damageUpgradeLevel w)
      MoveSpeedUpgrade -> (playerMoveSpeed w, speedUpgradeLevel w)
    price = upgradePrice level
    statLimit = case kind of
      MaxHealthUpgrade -> 200
      DamageUpgrade -> 80
      MoveSpeedUpgrade -> 400
    canAfford = goldCoins w >= price
    stateColor = if canAfford then Theme.cAccentGreen else Theme.cAccentRed
    stateLabel = if canAfford then "READY" else "NOT ENOUGH GOLD"

upgradePrice :: Int -> Int
upgradePrice currentLevel = 100 + max 0 currentLevel * 100

upgradeHit :: World -> (Float, Float) -> Maybe UpgradeKind
upgradeHit w mouse = firstHit upgrades
  where
    upgrades =
      [ (kind, (x + 120, y - 72))
      | (kind, x, y) <- upgradeChoices w
      ]
    firstHit [] = Nothing
    firstHit ((kind, position) : rest)
      | insideRect mouse position 110 44 = Just kind
      | otherwise = firstHit rest

upgradeChoices :: World -> [(UpgradeKind, Float, Float)]
upgradeChoices w =
  zipWith place (rotateChoices (upgradeRefreshes w) available) locations
  where
    available =
      [ MaxHealthUpgrade, DamageUpgrade, MoveSpeedUpgrade ]
    locations = [(-240, 100), (240, 100), (0, -105)]
    place kind (x, y) = (kind, x, y)

upgradeAsset :: UpgradeKind -> AssetId
upgradeAsset MaxHealthUpgrade = AssetHealth
upgradeAsset DamageUpgrade = AssetSkill
upgradeAsset MoveSpeedUpgrade = AssetSkill

upgradeName :: UpgradeKind -> String
upgradeName MaxHealthUpgrade = "MAX HP"
upgradeName DamageUpgrade = "ATTACK DAMAGE"
upgradeName MoveSpeedUpgrade = "MOVE SPEED"

rotateChoices :: Int -> [a] -> [a]
rotateChoices _ [] = []
rotateChoices count items =
  let offset = count `mod` length items
  in drop offset items ++ take offset items
