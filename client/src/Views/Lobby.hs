module Views.Lobby where

import Graphics.Gloss
import Assets
import Types
import Components.UI
import qualified Theme
import qualified Types.GameState as GS

drawLobby :: World -> Picture
drawLobby w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , Translate 0 300 $
      drawSoul3DText 2.5 Theme.cTextGold Theme.cDarkBorder "CO-OP LOBBY"
  , Translate (-405) 215 $ drawRoomPanel w
  , Translate 70 215 $ drawJoinPanel w
  , Translate 0 (-10) $ drawSlots w
  , Translate 0 (-175) $
      drawTextShadow 0.13 Theme.cAccentBlue Theme.cDarkBorder
        (if onlinePlayerId w == Nothing
          then "JOIN WITH A BLANK CODE TO CREATE A NEW ONLINE ROOM"
          else "ONLINE ROOM - THE HOST PRESSES START RUN")
  , button w (-450, -270) "BACK" 200
  , button w (-150, -270) (if isReady w then "NOT READY" else "READY UP") 220
  , button w (150, -270) "START RUN" 220
  , button w (450, -270) "CHOOSE HERO" 220
  , Translate 0 (-326) $
      drawTextShadow 0.12 Theme.cTextMuted Theme.cDarkBorder (statusText w)
  ]

drawRoomPanel :: World -> Picture
drawRoomPanel w = Pictures
  [ drawPanel 310 110
  , Translate 0 27 $
      drawSoul3DText 1.0 Theme.cTextMuted Theme.cDarkBorder "ROOM ID"
  , Translate 0 (-10) $
      drawSoul3DText 2.0 Theme.cTextGold Theme.cDarkBorder (roomCode w)
  , Translate 0 (-42) $
      drawTextShadow 0.11 Theme.cAccentGreen Theme.cDarkBorder (if onlinePlayerId w == Nothing then "NOT CONNECTED" else "ONLINE ROOM")
  , Translate 0 (-72) $ drawSoulButton 170 32 "COPY CODE"
      (insideRect (mousePos w) (-405, 143) 170 32)
  ]

drawJoinPanel :: World -> Picture
drawJoinPanel w = Pictures
  [ Translate (-70) 0 $ Pictures
      [ Color (if roomInputActive w then Theme.cPanelGlow else Theme.cPanelBorder) $
          rectangleSolid 320 58
      , Color Theme.cPanelBg $ rectangleSolid 314 52
      , Translate (-145) (-6) $
          drawTextShadow 0.14 Theme.cTextWhite Theme.cDarkBorder
            (if null (roomInput w) then "ROOM CODE (BLANK = CREATE)" else roomInput w)
      ]
  , Translate 145 0 $ drawSoulButton 130 58 "JOIN"
      (insideRect (mousePos w) (215, 215) 130 58)
  , Translate (-70) (-39) $
      drawTextShadow 0.10 Theme.cTextMuted Theme.cDarkBorder
        "JOIN CONNECTS TO THE GAME SERVER"
  ]

drawSlots :: World -> Picture
drawSlots w = case onlinePlayerId w of
  Just me -> Pictures
    [ onlineSlot me slot x
    | (slot, x) <- zip [1 ..] [-390, -130, 130, 390]
    ]
  Nothing -> drawMockSlots w
  where
    profs = GS.profiles (sharedState w)
    onlineSlot me slot x = case drop (slot - 1) profs of
      ((pid, prof) : _) ->
        drawPlayerSlot w x slot
          (if pid == me then "YOU" else take 8 (GS.profileName prof))
          (roleCharacter (GS.profileRole prof)) True False
      [] -> drawPlayerSlot w x slot "OPEN SLOT" Swordsman False False

roleCharacter :: GS.Role -> Character
roleCharacter GS.Archer = Archer
roleCharacter GS.Warrior = Swordsman

drawMockSlots :: World -> Picture
drawMockSlots w = Pictures
  [ drawPlayerSlot w (-390) 1 "YOU" (selectedChar w) True (isReady w)
  , drawPlayerSlot w (-130) 2 "ALLY 02" Archer True False
  , drawPlayerSlot w 130 3 "ALLY 03" Knight (isReady w) False
  , drawPlayerSlot w 390 4 "OPEN SLOT" Swordsman False False
  ]

drawPlayerSlot :: World -> Float -> Int -> String -> Character -> Bool -> Bool -> Picture
drawPlayerSlot w x slot name character connected ready =
  let hovered = insideRect (mousePos w) (x, -10) 230 245
      selectedBorder = if slot == 1 then Theme.cPrimaryGold else Theme.cPanelBorder
      statusColor = if ready then Theme.cAccentGreen else Theme.cTextMuted
  in Translate x (-10) $ Pictures
    [ Color (if hovered then Theme.cPanelGlow else selectedBorder) $
        rectangleSolid 230 245
    , Color Theme.cPanelBg $ rectangleSolid 224 239
    , Translate 0 84 $
        drawSoul3DText 0.95 Theme.cTextMuted Theme.cDarkBorder
          ("SLOT 0" ++ show slot)
    , drawAssetAt w (characterAsset character) (0, 30) (120, 135) Contain
    , drawAssetAt w AssetAvatarFrame (0, 30) (140, 150) Contain
    , Translate (-(fromIntegral (length name) * 4)) (-35) $
        drawSoul3DText 1.05 Theme.cTextWhite Theme.cDarkBorder name
    , Translate 0 (-62) $
        drawTextShadow 0.11 Theme.cTextGold Theme.cDarkBorder
          (characterName character)
    , Translate 0 (-91) $
        drawSoul3DText 0.9 statusColor Theme.cDarkBorder
          (if not connected then "WAITING" else if ready then "READY" else "ONLINE")
    ]

characterName :: Character -> String
characterName Swordsman = "SWORDSMAN"
characterName Archer = "ARCHER"
characterName Knight = "KNIGHT"
characterName Bomber = "BOMBER"
characterName Mage = "MAGE"

button :: World -> (Float, Float) -> String -> Float -> Picture
button w (x, y) label width = Translate x y $
  drawSoulButton width 52 label
    (insideRect (mousePos w) (x, y) width 52)
