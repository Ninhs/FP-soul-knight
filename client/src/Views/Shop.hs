module Views.Shop
  ( drawShop
  , shopItemHit
  , shopItemDetails
  ) where

import Graphics.Gloss
import Assets
import Types
import Components.UI
  ( drawAssetAt
  , drawAssetSlot
  , menuBackground
  , drawSoul3DText
  , drawSoulButton
  , drawTextShadow
  , drawPanel
  , insideRect
  , drawTitleAt
  , drawCenteredText
  )
import qualified Theme

drawShop :: World -> Picture
drawShop w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawPanel 1240 660
  , drawTitleAt 0 2.4 Theme.cTextGold Theme.cDarkBorder 300 "LOCAL SHOP DEMO"
  , Translate (-500) (-55) $ drawPanel 240 150
  , drawTitleAt (-500) 0.95 Theme.cTextGold Theme.cDarkBorder (-40) "TRADER"
  , drawCenteredText (-500) 0.10 Theme.cTextWhite Theme.cDarkBorder (-78)
      "Preview items only."
  , drawCenteredText (-500) 0.10 Theme.cTextWhite Theme.cDarkBorder (-100)
      "Purchases are not saved."
  , Translate 475 285 $ drawPanel 250 70
  , drawAssetAt w AssetCoin (390, 285) (28, 28) Contain
  , Translate 470 285 $
      drawSoul3DText 1.2 Theme.cTextGold Theme.cDarkBorder
        (show (goldCoins w) ++ " G")
  , Translate 20 240 $ drawSoulButton 190 44 "ITEMS"
      (not (shopEquipmentTab w)
        || insideRect (mousePos w) (20, 240) 190 44)
  , Translate 240 240 $ drawSoulButton 190 44 "EQUIPMENT"
      (shopEquipmentTab w
        || insideRect (mousePos w) (240, 240) 190 44)
  , drawCenteredText 130 0.10 Theme.cTextMuted Theme.cDarkBorder 205
      (if shopEquipmentTab w then "EQUIPMENT STOCK" else "CONSUMABLE STOCK")
  , Pictures (map (drawItem w) (shopItemsFor w))
  , Translate (-230) (-280) $ drawSoulButton 200 48 "BACK"
      (insideRect (mousePos w) (-230, -280) 200 48)
  , Translate 20 (-280) $ drawSoulButton 240 48 "REFRESH 5 GEMS"
      (insideRect (mousePos w) (20, -280) 240 48)
  , Translate 335 (-280) $
      drawTextShadow 0.12 Theme.cTextWhite Theme.cDarkBorder
        ("GEMS: " ++ show (gems w))
  ]

drawItem :: World -> (ShopItemKind, Float) -> Picture
drawItem w (kind, x) =
  let (title, description, price, _) = shopItemDetails kind
      canAfford = goldCoins w >= price
      iconPosition = (x, 100)
      buttonPosition = (x, -100)
  in Pictures
    [ Translate x 15 $ drawPanel 200 350
    , drawAssetAt w (shopItemAsset kind) iconPosition (76, 76) Contain
    , drawTitleAt x 1.0 Theme.cTextWhite Theme.cDarkBorder 50 title
    , drawCenteredText x 0.10 Theme.cTextMuted Theme.cDarkBorder 15 description
    , drawCenteredText x 0.11
        (if canAfford then Theme.cTextGold else Theme.cAccentRed)
        Theme.cDarkBorder (-28) (show price ++ " GOLD")
    , drawCenteredText x 0.10 Theme.cTextMuted Theme.cDarkBorder (-62)
        ("OWNED: " ++ show (inventoryCount kind (shopInventory w)))
    , Translate x (-100) $ drawSoulButton 140 40 "BUY"
        (insideRect (mousePos w) buttonPosition 140 40)
    ]

shopItemAsset :: ShopItemKind -> AssetId
shopItemAsset SmallHealthPotion = AssetHealthPotion
shopItemAsset LightArmor = AssetLightArmor
shopItemAsset LargeHealthPotion = AssetLargePotion
shopItemAsset HeavyArmor = AssetHeavyArmor
shopItemAsset DamageCharm = AssetDamageRune
shopItemAsset SwiftBoots = AssetSwiftBoots

shopItemsFor :: World -> [(ShopItemKind, Float)]
shopItemsFor w = zip selectedItems [-200, 20, 240, 460]
  where
    stock
      | shopEquipmentTab w =
          [DamageCharm, SwiftBoots]
      | otherwise =
          [SmallHealthPotion, LargeHealthPotion]
    selectedItems = rotateItems (shopRefreshes w) stock

rotateItems :: Int -> [a] -> [a]
rotateItems _ [] = []
rotateItems count items =
  let offset = count `mod` length items
  in drop offset items ++ take offset items

shopItemDetails :: ShopItemKind -> (String, String, Int, (Float, Float))
shopItemDetails SmallHealthPotion =
  ("SMALL MEDKIT", "RESTORE +25 HP", 100, (25, 0))
shopItemDetails LargeHealthPotion =
  ("LARGE MEDKIT", "RESTORE +60 HP", 220, (60, 0))
shopItemDetails DamageCharm =
  ("DAMAGE CHARM", "+3 ATTACK DAMAGE", 350, (0, 0))
shopItemDetails SwiftBoots =
  ("SWIFT BOOTS", "+20 MOVE SPEED", 300, (0, 0))

shopItemHit :: World -> (Float, Float) -> Maybe ShopItemKind
shopItemHit w mouse = firstHit (shopItemsFor w)
  where
    firstHit [] = Nothing
    firstHit ((kind, x) : rest)
      | insideRect mouse (x, -100) 140 40 = Just kind
      | otherwise = firstHit rest

inventoryCount :: ShopItemKind -> [(ShopItemKind, Int)] -> Int
inventoryCount kind inventory = sum
  [ count | (item, count) <- inventory, item == kind ]
