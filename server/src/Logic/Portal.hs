module Logic.Portal
  ( Vec2
  , isInsidePortal
  , allMonstersDead
  , allRequiredPlayersInside
  , canAdvancePortal
  ) where

import Logic.Movement (Vec2, distance)

isInsidePortal :: Vec2 -> Vec2 -> Float -> Bool
isInsidePortal playerPos portalPos radius =
  distance playerPos portalPos <= max 0 radius

-- Hết quái khi không còn con nào sống. isAlive do bạn truyền vào (mHp > 0...).
allMonstersDead :: (m -> Bool) -> [m] -> Bool
allMonstersDead isAlive = not . any isAlive

-- Mọi người chơi "bắt buộc" đã đứng trong cổng.
-- isRequired: chỉ lấy người còn sống và còn kết nối, người đã chết hoặc
-- mất kết nối không được chặn cổng.
-- Không có người bắt buộc nào thì trả False (không ai thì không qua map).
allRequiredPlayersInside
  :: (p -> Bool) -> (p -> Vec2) -> Vec2 -> Float -> [p] -> Bool
allRequiredPlayersInside isRequired getPos portalPos radius players =
  case filter isRequired players of
    []       -> False
    required -> all (\p -> isInsidePortal (getPos p) portalPos radius) required

-- Cổng cho qua khi hết quái và mọi người chơi bắt buộc đã vào cổng.
canAdvancePortal :: Bool -> Bool -> Bool
canAdvancePortal monstersAllDead playersAllInside =
  monstersAllDead && playersAllInside
