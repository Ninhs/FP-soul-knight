module Views.Shop
  ( drawShop
  , shopItemHit
  , shopItemDetails
  ) where

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

drawShop :: World -> Picture
drawShop w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , drawPanel 1200 620
  , Translate 0 300 $
      drawSoul3DText 2.4 Theme.cTextGold Theme.cDarkBorder "DUNGEON SHOP"
  , drawAssetSlot w SlotShopkeeper
  , Translate (-470) (-55) $ drawPanel 260 150
  , Translate (-470) (-35) $
      drawSoul3DText 0.95 Theme.cTextGold Theme.cDarkBorder "TRADER"
  , Translate (-470) (-72) $
      drawTextShadow 0.11 Theme.cTextWhite Theme.cDarkBorder
        "Choose supplies for your next run."
  , Translate 475 300 $ drawPanel 250 70
  , drawAssetAt w AssetCoin (390, 300) (28, 28) Contain
  , Translate 470 300 $
      drawSoul3DText 1.2 Theme.cTextGold Theme.cDarkBorder
        (show (goldCoins w) ++ " G")
  , Translate (-70) 220 $ drawSoulButton 190 44 "ITEMS"
      (not (shopEquipmentTab w)
        || insideRect (mousePos w) (-70, 220) 190 44)
  , Translate 150 220 $ drawSoulButton 190 44 "EQUIPMENT"
      (shopEquipmentTab w
        || insideRect (mousePos w) (150, 220) 190 44)
  , Translate (-70) 180 $
      drawSoul3DText 1.0 Theme.cTextMuted Theme.cDarkBorder
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
    [ Translate x 15 $ drawPanel 190 350
    , drawAssetAt w (shopItemAsset kind) iconPosition (76, 76) Contain
    , Translate x 50 $
        drawSoul3DText 1.05 Theme.cTextWhite Theme.cDarkBorder title
    , Translate x 15 $
        drawTextShadow 0.10 Theme.cTextMuted Theme.cDarkBorder description
    , Translate x (-28) $
        drawSoul3DText 1.0
          (if canAfford then Theme.cTextGold else Theme.cAccentRed)
          Theme.cDarkBorder (show price ++ " GOLD")
    , Translate x (-62) $
        drawTextShadow 0.10 Theme.cTextMuted Theme.cDarkBorder
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
shopItemsFor w = zip selectedItems [-270, -50, 170, 390]
  where
    stock
      | shopEquipmentTab w =
          [DamageCharm, SwiftBoots, LightArmor, HeavyArmor]
      | otherwise =
          [SmallHealthPotion, LargeHealthPotion, LightArmor, HeavyArmor]
    selectedItems = rotateItems (shopRefreshes w) stock

rotateItems :: Int -> [a] -> [a]
rotateItems _ [] = []
rotateItems count items =
  let offset = count `mod` length items
  in drop offset items ++ take offset items

shopItemDetails :: ShopItemKind -> (String, String, Int, (Float, Float))
shopItemDetails SmallHealthPotion =
  ("SMALL MEDKIT", "RESTORE +25 HP", 100, (25, 0))
shopItemDetails LightArmor =
  ("LIGHT ARMOR", "RESTORE +20 ARMOR", 120, (0, 20))
shopItemDetails LargeHealthPotion =
  ("LARGE MEDKIT", "RESTORE +60 HP", 220, (60, 0))
shopItemDetails HeavyArmor =
  ("HEAVY ARMOR", "RESTORE +50 ARMOR", 250, (0, 50))
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
