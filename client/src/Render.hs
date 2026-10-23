module Render (renderGame) where

import Graphics.Gloss
import Types.GameState

renderGame :: GameState -> Picture
renderGame gs = case phase gs of
  Lobby        -> renderLobby gs
  Playing ms   -> renderMap ms
  RoomResult r -> renderResult r

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