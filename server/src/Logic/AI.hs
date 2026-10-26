
{-# LANGUAGE BangPatterns #-}

module Logic.AI
  ( Vec2
  , nearestBy
  , nearestWithinRange
  , retainTarget
  , directionToTarget
  , directionAwayFromTarget
  , shouldChase
  , shouldAttack
  , AIState(..)
  , RangedMove(..)
  , decideAIState
  , rangedMove
  , decideRangedState
  , moveDirectionFor
  , moveDirectionWithRange
  , predictTargetPosition
  , patrolDirection
  , monsterAggroRange
  , monsterAttackRange
  , selectTargetForMonster
  , monsterAIState
  , monsterMoveDirection
  , moveMonsterWithCollision
  ) where

import Data.List (find)

import Logic.Movement
  ( Vec2
  , addVec
  , distanceSq
  , normalize
  , scaleVec
  , subVec
  , withinDistance
  , stepFromInput
  , moveWithCollisionSteps
  )

import Types.GameState

rangeSq :: Float -> Float
rangeSq range =
  let r = max 0 range
  in r * r
{-# INLINE rangeSq #-}

-- Chọn mục tiêu gần nhất, giữ nguyên toàn bộ dữ liệu và ID.
nearestBy :: (a -> Vec2) -> Vec2 -> [a] -> Maybe a
nearestBy _ _ [] = Nothing
nearestBy getPos origin (first:rest) =
  Just (go first (distTo first) rest)
  where
    distTo target = distanceSq origin (getPos target)

    go !best !_ [] = best
    go !best !bestD (target:targets)
      | d < bestD = go target d targets
      | otherwise = go best bestD targets
      where
        d = distTo target

-- Tìm mục tiêu gần nhất trong phạm vi phát hiện.
nearestWithinRange
  :: (a -> Vec2) -> Vec2 -> Float -> [a] -> Maybe a
nearestWithinRange getPos origin range = go Nothing
  where
    limitSq = rangeSq range

    go !best [] = fmap fst best
    go !best (target:targets)
      | d > limitSq = go best targets
      | otherwise =
          case best of
            Just (_, bestD) | bestD <= d ->
              go best targets
            _ -> go (Just (target, d)) targets
      where
        d = distanceSq origin (getPos target)

-- Giữ mục tiêu hiện tại nếu vẫn sống và còn trong tầm.
retainTarget
  :: (a -> Int)
  -> Int
  -> (a -> Vec2)
  -> Vec2
  -> Float
  -> [a]
  -> Maybe a
retainTarget getId currentId getPos origin aggroRange candidates =
  case find ((== currentId) . getId) candidates of
    Just target
      | withinDistance aggroRange origin (getPos target) ->
          Just target
    _ -> nearestWithinRange getPos origin aggroRange candidates

directionToTarget :: Vec2 -> Vec2 -> Vec2
directionToTarget fromPos targetPos =
  normalize (subVec targetPos fromPos)

directionAwayFromTarget :: Vec2 -> Vec2 -> Vec2
directionAwayFromTarget fromPos targetPos =
  case normalize (subVec fromPos targetPos) of
    (0, 0) -> (1, 0)
    direction -> direction

shouldChase :: Float -> Vec2 -> Vec2 -> Bool
shouldChase = withinDistance

shouldAttack :: Float -> Vec2 -> Vec2 -> Bool
shouldAttack = withinDistance

data AIState
  = Idle
  | Chasing
  | Attacking
  | KeepingDistance
  deriving (Eq, Show, Read)

data RangedMove
  = Retreat
  | Approach
  | Hold
  deriving (Eq, Show, Read)

-- Nothing nghĩa là không có mục tiêu hợp lệ.
decideAIState :: Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideAIState _ _ Nothing _ = Idle
decideAIState aggroRange attackRange (Just targetPos) monsterPos
  | dSq > rangeSq aggroRange = Idle
  | dSq <= rangeSq attackRange = Attacking
  | otherwise = Chasing
  where
    dSq = distanceSq monsterPos targetPos

-- Quái tầm xa: lùi khi quá gần, tiến khi quá xa.
rangedMove :: Float -> Float -> Vec2 -> Vec2 -> RangedMove
rangedMove minimumRange preferredRange monsterPos targetPos
  | dSq < rangeSq minRange = Retreat
  | dSq > rangeSq maxRange = Approach
  | otherwise = Hold
  where
    dSq = distanceSq monsterPos targetPos
    minRange = max 0 minimumRange
    maxRange = max minRange preferredRange

decideRangedState
  :: Float -> Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideRangedState _ _ _ Nothing _ = Idle
decideRangedState aggroRange minimumRange preferredRange
                 (Just targetPos) monsterPos
  | not (shouldChase aggroRange monsterPos targetPos) = Idle
  | otherwise =
      case rangedMove minimumRange preferredRange monsterPos targetPos of
        Retreat -> KeepingDistance
        Approach -> Chasing
        Hold -> Attacking

moveDirectionFor :: AIState -> Vec2 -> Vec2 -> Vec2
moveDirectionFor state monsterPos targetPos =
  case state of
    Chasing -> directionToTarget monsterPos targetPos
    KeepingDistance -> directionAwayFromTarget monsterPos targetPos
    _ -> (0, 0)

-- Dừng khi đạt khoảng cách mong muốn.
moveDirectionWithRange
  :: AIState -> Float -> Vec2 -> Vec2 -> Vec2
moveDirectionWithRange state desiredRange monsterPos targetPos =
  case state of
    Chasing
      | dSq > stopSq -> directionToTarget monsterPos targetPos
      | otherwise -> (0, 0)
    KeepingDistance
      | dSq < stopSq -> directionAwayFromTarget monsterPos targetPos
      | otherwise -> (0, 0)
    _ -> (0, 0)
  where
    dSq = distanceSq monsterPos targetPos
    stopSq = rangeSq desiredRange

-- Dự đoán vị trí mục tiêu theo vận tốc.
predictTargetPosition :: Vec2 -> Vec2 -> Float -> Vec2
predictTargetPosition targetPos targetVelocity predictionSeconds =
  addVec targetPos
    (scaleVec (max 0 predictionSeconds) targetVelocity)

-- Tuần tra tới waypoint.
patrolDirection :: Vec2 -> Vec2 -> Float -> Vec2
patrolDirection currentPos waypoint stopDistance
  | withinDistance stopDistance currentPos waypoint = (0, 0)
  | otherwise = directionToTarget currentPos waypoint

-- Phạm vi phát hiện theo loại quái.
monsterAggroRange :: Monster -> Float
monsterAggroRange monster =
  case kind monster of
    Boss -> 12.0
    Regular Ranged _ -> 10.0
    Regular _ TankBody -> 8.0
    Regular _ GlassBody -> 11.0
    Regular _ _ -> 9.0

-- Phạm vi tấn công theo loại quái.
monsterAttackRange :: Monster -> Float
monsterAttackRange monster =
  case kind monster of
    Boss -> 1.8
    Regular Ranged _ -> 7.0
    Regular _ _ -> 1.2

-- Chỉ chọn người chơi sống và còn HP.
selectTargetForMonster :: Monster -> [Player] -> Maybe Player
selectTargetForMonster monster playersNow =
  nearestWithinRange
    position
    (mPosition monster)
    (monsterAggroRange monster)
    (filter canTarget playersNow)
  where
    canTarget player =
      status player == Alive && hp (stats player) > 0

monsterAIState :: Monster -> [Player] -> AIState
monsterAIState monster playersNow =
  case selectTargetForMonster monster playersNow of
    Nothing -> Idle
    Just target ->
      let targetPos = position target
          monsterPos = mPosition monster
      in case kind monster of
           Regular Ranged _ ->
             decideRangedState
               (monsterAggroRange monster)
               (rangedMinimum monster)
               (rangedPreferred monster)
               (Just targetPos)
               monsterPos
           _ ->
             decideAIState
               (monsterAggroRange monster)
               (monsterAttackRange monster)
               (Just targetPos)
               monsterPos
  where
    rangedMinimum m = min 3.5 (monsterAttackRange m * 0.55)
    rangedPreferred m =
      max (rangedMinimum m + 0.5)
          (min 7.0 (monsterAttackRange m))

monsterMoveDirection :: Monster -> [Player] -> Vec2
monsterMoveDirection monster playersNow =
  case selectTargetForMonster monster playersNow of
    Nothing -> (0, 0)
    Just target ->
      let monsterPos = mPosition monster
          targetPos = position target
          state = monsterAIState monster playersNow
          stopRange =
            case kind monster of
              Regular Ranged _ -> rangedPreferred monster
              _ -> monsterAttackRange monster
      in moveDirectionWithRange
           state stopRange monsterPos targetPos
  where
    rangedPreferred m =
      max 4.0 (min 7.0 (monsterAttackRange m))

-- Cập nhật vị trí quái với va chạm do server cung cấp.
-- isBlocked phải xét cả hitbox của quái với tường.
moveMonsterWithCollision
  :: (Vec2 -> Bool)
  -> Float
  -> [Player]
  -> Monster
  -> Monster
moveMonsterWithCollision isBlocked dt playersNow monster
  | hp (mStats monster) <= 0 = monster
  | not (any canTarget playersNow) = monster
  | otherwise =
      monster
        { mPosition =
            moveWithCollisionSteps
              isBlocked
              0.2
              (mPosition monster)
              delta
        }
  where
    elapsed = max 0 (min 0.25 dt)
    direction = monsterMoveDirection monster playersNow
    baseSpeed = max 0 (moveSpeed (mStats monster))

    speed =
      case kind monster of
        Regular _ TankBody -> baseSpeed * 0.65
        Regular _ GlassBody -> baseSpeed * 1.25
        Regular Ranged _ -> baseSpeed * 0.8
        Boss -> baseSpeed
        _ -> baseSpeed

    delta = stepFromInput direction speed elapsed

    canTarget player =
      status player == Alive && hp (stats player) > 0
