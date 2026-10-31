module Views.Lobby where

import Data.List (sortOn)
import Graphics.Gloss
import Assets
import Types
import Components.UI
import qualified Theme
import qualified Types.GameState as GS

-- Trạng thái phòng nhìn từ máy mình: ai là chủ phòng (id nhỏ nhất), mình đã sẵn sàng chưa...
lobbyProfiles :: World -> [(Int, GS.PlayerProfile)]
lobbyProfiles w = sortOn fst (GS.profiles (sharedState w))

isHostHere :: World -> Bool
isHostHere w = case (onlinePlayerId w, lobbyProfiles w) of
  (Just me, (hostId, _) : _) -> me == hostId
  _ -> False

myReady :: World -> Bool
myReady w = or [GS.profileReady p | (i, p) <- lobbyProfiles w, Just i == onlinePlayerId w]

drawLobby :: World -> Picture
drawLobby w = Pictures
  [ menuBackground w
  , Color Theme.cOverlay $ rectangleSolid Theme.screenWidth Theme.screenHeight
  , Translate 0 300 $
      drawSoul3DText 2.5 Theme.cTextGold Theme.cDarkBorder "CO-OP LOBBY"
  , Translate (-405) 215 $ drawRoomPanel w
  , Translate 0 215 $ drawJoinPanel w
  , drawSlots w
  , ctext 0 0.13 Theme.cAccentBlue Theme.cDarkBorder (-180) hint
  , button w (-450, -270) "BACK" 200
  , middleButton
  , button w (450, -270) "CHOOSE HERO" 220
  , ctext 0 0.12 Theme.cTextMuted Theme.cDarkBorder (-326) (take 80 (statusText w))
  ]
  where
    online = onlinePlayerId w /= Nothing
    middleButton
      | not online = Blank
      | isHostHere w = button w (0, -270) "START RUN" 260
      | otherwise = button w (0, -270) (if myReady w then "NOT READY" else "READY UP") 260
    hint
      | not online = "JOIN WITH A BLANK CODE TO CREATE A NEW ONLINE ROOM"
      | isHostHere w = "YOU ARE THE HOST - START WHEN EVERY PLAYER IS READY"
      | otherwise = "PRESS READY UP - THE HOST WILL START THE RUN"

drawRoomPanel :: World -> Picture
drawRoomPanel w = Pictures
  [ drawPanel 310 110
  , ctext 0 0.13 Theme.cTextMuted Theme.cDarkBorder 22 "ROOM ID"
  , ctext 0 0.26 Theme.cTextGold Theme.cDarkBorder (-14) (roomCode w)
  , ctext 0 0.11 Theme.cAccentGreen Theme.cDarkBorder (-42)
      (if onlinePlayerId w == Nothing then "NOT CONNECTED" else "ONLINE ROOM")
  , Translate 0 (-72) $ drawSoulButton 170 32 "COPY CODE"
      (insideRect (mousePos w) (-405, 143) 170 32)
  ]

-- Vẽ quanh gốc (0, 215): ô mã phòng, nút JOIN, ô tên (chỉnh toạ độ chuột ở ClientUI/Main cho khớp)
drawJoinPanel :: World -> Picture
drawJoinPanel w = Pictures
  [ Pictures
      [ Color (if roomInputActive w then Theme.cPanelGlow else Theme.cPanelBorder) $
          rectangleSolid 320 58
      , Color Theme.cPanelBg $ rectangleSolid 314 52
      , Translate (-148) (-6) $
          drawTextShadow 0.12 Theme.cTextWhite Theme.cDarkBorder
            (if null (roomInput w) then "ROOM CODE (BLANK = NEW)" else roomInput w)
      ]
  , Translate 235 0 $ drawSoulButton 130 58 "JOIN"
      (insideRect (mousePos w) (235, 215) 130 58)
  , Translate 0 (-50) $ Pictures
      [ Color (if nameInputActive w then Theme.cPanelGlow else Theme.cPanelBorder) $
          rectangleSolid 320 34
      , Color Theme.cPanelBg $ rectangleSolid 314 28
      , Translate (-148) (-5) $
          drawTextShadow 0.12 Theme.cTextWhite Theme.cDarkBorder
            (if null (playerName w) then "YOUR NAME (CLICK TO TYPE)" else playerName w)
      ]
  ]

-- Luôn có 4 ô. Chủ phòng đứng đầu (viền đỏ), người vào sau viền vàng, ô chưa có ai thì để trống.
drawSlots :: World -> Picture
drawSlots w = Pictures
  [ drawSlot w x slot occupant
  | (slot, x, occupant) <- zip3 [1 ..] [-390, -130, 130, 390] occupants
  ]
  where
    occupants =
      [ Just (take 10 (GS.profileName prof), roleCharacter (GS.profileRole prof), slot == 1, GS.profileReady prof, Just pid == onlinePlayerId w)
      | (slot, (pid, prof)) <- zip [1 :: Int ..] (if onlinePlayerId w == Nothing then [] else lobbyProfiles w)
      ] ++ repeat Nothing

roleCharacter :: GS.Role -> Character
roleCharacter GS.Archer = Archer
roleCharacter GS.Warrior = Swordsman

drawSlot :: World -> Float -> Int -> Maybe (String, Character, Bool, Bool, Bool) -> Picture
drawSlot w x slot occupant = Translate x (-10) $ Pictures $
  [ Color border $ rectangleSolid 230 245
  , Color Theme.cPanelBg $ rectangleSolid 224 239
  , ctext 0 0.12 Theme.cTextMuted Theme.cDarkBorder 100 ("SLOT 0" ++ show slot ++ mineTag)
  ] ++ body
  where
    mineTag = case occupant of
      Just (_, _, _, _, True) -> " - YOU"
      _ -> ""
    border = case occupant of
      Just (_, _, True, _, _) -> Theme.cAccentRed
      Just _ -> Theme.cPrimaryGold
      Nothing -> Theme.cPanelBorder
    body = case occupant of
      Nothing ->
        [ ctext 0 0.14 Theme.cTextMuted Theme.cDarkBorder (-6) "OPEN SLOT"
        , ctext 0 0.10 Theme.cTextMuted Theme.cDarkBorder (-30) "WAITING FOR PLAYER"
        ]
      Just (name, character, host, ready, mine) ->
        [ drawAssetAt w (characterAsset character) (0, 28) (96, 108) Contain
        , drawAssetAt w AssetAvatarFrame (0, 28) (130, 140) Contain
        , ctext 0 0.15 Theme.cTextWhite Theme.cDarkBorder (-62) name
        , ctext 0 0.11 Theme.cTextGold Theme.cDarkBorder (-84) (characterName character)
        , ctext 0 0.13 (if host then Theme.cAccentRed else if ready then Theme.cAccentGreen else Theme.cTextMuted)
            Theme.cDarkBorder (-108) (if host then "HOST" else if ready then "READY" else "NOT READY")
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

-- Căn giữa chữ: ước lượng độ rộng ký tự của font gloss (~82/100) chính xác hơn drawCenteredText.
ctext :: Float -> Float -> Color -> Color -> Float -> String -> Picture
ctext cx size textColor shadowColor y label =
  drawCenteredText (cx + 9 * fromIntegral (length label) * size) size textColor shadowColor y label
