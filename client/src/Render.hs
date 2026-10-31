module Render (renderGame, renderClient) where

import Graphics.Gloss
import Animation (animKind, currentFrame)
import Sprites (framesOf)
import Components.UI (drawPauseOverlay, menuBackground)
import ClientUI (toEnemyData, toScreenPosition)
import Types
import Types.GameState
import qualified Types.GameState as GS
import Views.CharSelect (drawCharSelect)
import Views.HUD (drawChestRewardOverlay, drawGameHUD, drawTutorialGuide)
import Views.Lobby (drawLobby)
import Views.MainMenu (drawMainMenu)
import Views.MapSelect (drawMapSelect)
import Views.Overlays (drawConfirmMenu, drawGameResult, drawShopNotice)
import Views.Shop (drawShop)
import Views.Upgrade (drawUpgrade)

renderGame :: GameState -> Picture
renderGame gs = case phase gs of
  Lobby        -> renderLobby gs
  Playing ms   -> renderMap ms
  RoomResult r -> renderResult r

renderClient :: ClientState -> Picture
renderClient state = case appView state of
  MainMenuView -> drawMainMenu state
  LobbyView -> drawLobby state
  CharacterSelectView -> drawCharSelect state
  MapSelectView -> drawMapSelect state
  TutorialView -> drawTutorialGuide state
  InGameView -> Pictures [drawGameHUD (combatWorld state), drawAllies state]
  ShopView -> drawShop state
  UpgradeView -> drawUpgrade state
  PausedView -> Pictures
    [ drawGameHUD (combatWorld state)
    , drawPauseOverlay (mousePos state)
    ]
  ChestRewardView -> Pictures
    [ drawGameHUD (combatWorld state)
    , drawChestRewardOverlay state (ChestReward 120 25 10) (mousePos state)
    ]
  ShopNoticeView -> Pictures
    [ overlayBackground state
    , drawShopNotice (noticeText state) (mousePos state)
    ]
  ConfirmMenuView -> Pictures
    [ drawGameHUD (combatWorld state)
    , drawConfirmMenu (mousePos state)
    ]
  VictoryView -> Pictures [menuBackground state, drawGameResult True state]
  DefeatView -> Pictures [menuBackground state, drawGameResult False state]

overlayBackground :: ClientState -> Picture
overlayBackground state = case returnView state of
  MainMenuView -> drawMainMenu state
  LobbyView -> drawLobby state
  CharacterSelectView -> drawCharSelect state
  MapSelectView -> drawMapSelect state
  TutorialView -> drawTutorialGuide state
  InGameView -> drawGameHUD (combatWorld state)
  ShopView -> drawShop state
  UpgradeView -> drawUpgrade state
  _ -> menuBackground state

combatWorld :: ClientState -> World
combatWorld state =
  let localMap = localMapState state
      syncedEnemies = enemies state
  in state
    { playerPos = playerPosition state
    , enemies = syncedEnemies
    , playerMaxHealth = max 1 (characterMaxHp (characterStats (selectedChar state)))
    , projectiles =
        [ ProjectileData (toScreenPosition (GS.bPosition bullet))
        | bullet <- GS.bullets localMap
        ]
    }

renderLobby :: GameState -> Picture
renderLobby gs = Translate (-250) 0 $ Scale 0.25 0.25 $ Color white $
  Text ("Cho nguoi choi... (" ++ show (length (profiles gs)) ++ "/4)")

renderMap :: MapState -> Picture
renderMap ms = Pictures
  ( map renderPlayer (players ms)
  ++ map renderMonster (monsters ms)
  ++ map renderBullet (bullets ms)
  ++ map renderDrop (drops ms)
  ++ [renderPortal (portal ms)]
  )

renderPlayer :: Player -> Picture
renderPlayer p =
  let (x, y) = position p
      col = case status p of
        Alive        -> blue
        Dead         -> greyN 0.5
        Disconnected -> greyN 0.2
  in Translate x y $ Color col $ circleSolid 15

renderMonster :: Monster -> Picture
renderMonster m =
  let (x, y) = mPosition m
      col = case kind m of
        Boss -> red
        _    -> orange
  in Translate x y $ Color col $ circleSolid 18

renderBullet :: Bullet -> Picture
renderBullet b =
  let (x, y) = bPosition b
  in Translate x y $ Color yellow $ circleSolid 4

renderDrop :: Drop -> Picture
renderDrop d =
  let (x, y) = dPosition d
      col = case dType d of
        GoldDrop _ -> yellow
        ExpDrop  _ -> green
        HealDrop _ -> rose
  in Translate x y $ Color col $ circleSolid 6

renderPortal :: Portal -> Picture
renderPortal p =
  let (x, y) = portalPos p
      col = if portalActive p then green else greyN 0.4
  in Translate x y $ Color col $ thickCircle 20 3

renderResult :: Result -> Picture
renderResult Victory = Scale 0.4 0.4 $ Color green $ Text "CHIEN THANG"
renderResult Defeat  = Scale 0.4 0.4 $ Color red   $ Text "THUA CUOC"

-- Vẽ đồng đội khi chơi online bằng sprite (người chơi của mình do drawGameHUD vẽ).
-- Toạ độ trừ đi vị trí mình để khớp camera của drawGameHUD.
drawAllies :: ClientState -> Picture
drawAllies state = Translate (negate px) (negate py) $ Pictures (map drawAlly (allies state))
  where
    (px, py) = playerPosition state
    drawAlly a =
      let (x, y) = allyPos a
          frames = framesOf (appAssets state) (allySprite a) (animKind (allyAnim a))
          frame = currentFrame frames (allyAnim a)
          s = 56 / 32
          sx = if allyFacingRight a then s else negate s
      in Translate x y $ Pictures
           [ Scale sx s frame
           , Translate (-10) 40 $ Scale 0.1 0.1 $ Color white $ Text (allyName a)
           ]
