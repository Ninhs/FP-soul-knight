module Logic.Portal
  ( portalRadius
  , isBossDefeated
  , updatePortal
  , playerInsidePortal
  , allRequiredPlayersInside
  , canExitPortal
  , roomOutcome
  ) where

import Logic.Combat (circlesOverlap, isMonsterAlive, isPlayerAlive, playerRadius)
import Types.GameState

-- Bán kính hitbox cổng (cùng hệ đơn vị với playerRadius, bulletRadius).
portalRadius :: Float
portalRadius = 1.0

isBoss :: Monster -> Bool
isBoss m = case kind m of
    Boss -> True
    _    -> False

-- Boss đã bị hạ khi không còn Boss nào còn sống.
-- (Boss chết đã bị splitDeadMonsters tách khỏi danh sách cũng tính là đã hạ.)
isBossDefeated :: [Monster] -> Bool
isBossDefeated = not . any (\m -> isBoss m && isMonsterAlive m)

-- Boss chết thì mở cổng
updatePortal :: MapState -> MapState
updatePortal ms
    | portalActive pt                  = ms
    | isBossDefeated (monsters ms)     = ms { portal = pt { portalActive = True } }
    | otherwise                        = ms
  where pt = portal ms

-- Hitbox người chơi chạm hitbox cổng.
playerInsidePortal :: Portal -> Player -> Bool
playerInsidePortal pt p =
    circlesOverlap (position p) playerRadius (portalPos pt) portalRadius

-- Mọi người chơi còn sống đều trong cổng. Người chết / mất kết nối không chặn cổng.
-- Không còn ai sống thì False.
allRequiredPlayersInside :: Portal -> [Player] -> Bool
allRequiredPlayersInside pt ps = case filter isPlayerAlive ps of
    []       -> False
    required -> all (playerInsidePortal pt) required

-- Được thoát: cổng đã mở và tất cả người chơi sống đã đứng trong cổng.
canExitPortal :: MapState -> Bool
canExitPortal ms =
    portalActive (portal ms) && allRequiredPlayersInside (portal ms) (players ms)

-- Kết cục sau một tick (Nothing = chưa kết thúc).
--   Không còn ai sống     -> Defeat
--   Cổng thoát được       -> Victory
roomOutcome :: MapState -> Maybe Result
roomOutcome ms
    | not (any isPlayerAlive (players ms)) = Just Defeat
    | canExitPortal ms                     = Just Victory
    | otherwise                            = Nothing
