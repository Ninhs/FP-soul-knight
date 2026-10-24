module Logic.AI
  ( Vec2
  , nearestBy
  , directionToTarget
  , shouldChase
  , AIState(..)
  , RangedMove(..)
  , decideAIState
  , rangedMove
  , decideRangedState
  , moveDirectionFor
  ) where

import Logic.Movement (Vec2, distance, normalize, scaleVec, subVec)

-- Chọn mục tiêu gần nhất, giữ nguyên cả giá trị (id, player...).
-- Chỉ truyền vào người chơi đủ điều kiện (còn sống, còn kết nối).
nearestBy :: (a -> Vec2) -> Vec2 -> [a] -> Maybe a
nearestBy _ _ [] = Nothing
nearestBy getPos origin (first:rest) =
  Just (foldl nearer first rest)
  where
    nearer best candidate
      | distance origin (getPos candidate) < distance origin (getPos best) = candidate
      | otherwise = best

directionToTarget :: Vec2 -> Vec2 -> Vec2
directionToTarget fromPos targetPos = normalize (subVec targetPos fromPos)

shouldChase :: Float -> Vec2 -> Vec2 -> Bool
shouldChase aggroRange fromPos targetPos =
  distance fromPos targetPos <= max 0 aggroRange

data AIState
  = Idle
  | Chasing
  | Attacking
  | KeepingDistance
  deriving (Eq, Show, Read)

-- Quái cận chiến.
-- aggroRange, attackRange, mục tiêu (Nothing = không có), vị trí quái.
-- Cooldown và thời gian khóa mục tiêu do GameLoop lưu trong state quái.
decideAIState :: Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideAIState _ _ Nothing _ = Idle
decideAIState aggroRange attackRange (Just targetPos) monsterPos
  | not (shouldChase aggroRange monsterPos targetPos) = Idle
  | distance monsterPos targetPos <= max 0 attackRange = Attacking
  | otherwise = Chasing

data RangedMove
  = Retreat   -- quá gần: lùi ra
  | Approach  -- quá xa: tiến lại
  | Hold      -- đúng tầm: đứng yên và bắn
  deriving (Eq, Show)

-- Quái tầm xa: gần hơn minimumRange thì lùi, xa hơn preferredRange thì tiến.
rangedMove :: Float -> Float -> Vec2 -> Vec2 -> RangedMove
rangedMove minimumRange preferredRange monsterPos targetPos
  | d < max 0 minimumRange = Retreat
  | d > max minimumRange preferredRange = Approach
  | otherwise = Hold
  where d = distance monsterPos targetPos

-- Quái tầm xa: aggroRange, minimumRange, preferredRange, mục tiêu, vị trí quái.
decideRangedState :: Float -> Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideRangedState _ _ _ Nothing _ = Idle
decideRangedState aggroRange minimumRange preferredRange (Just targetPos) monsterPos
  | not (shouldChase aggroRange monsterPos targetPos) = Idle
  | otherwise =
      case rangedMove minimumRange preferredRange monsterPos targetPos of
        Retreat  -> KeepingDistance
        Approach -> Chasing
        Hold     -> Attacking

-- Hướng đi theo trạng thái: đuổi thì tới mục tiêu, giữ khoảng cách thì lùi ra,
-- còn lại đứng yên. Kết quả đưa vào moveWithAxisCollision (nhân tốc độ * dt).
moveDirectionFor :: AIState -> Vec2 -> Vec2 -> Vec2
moveDirectionFor st monsterPos targetPos =
  case st of
    Chasing         -> toward
    KeepingDistance -> scaleVec (-1) toward
    _               -> (0, 0)
  where toward = directionToTarget monsterPos targetPos
