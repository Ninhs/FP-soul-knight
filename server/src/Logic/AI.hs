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
  ) where

import Data.List (find)

import Logic.Movement
  ( Vec2, addVec, distanceSq, normalize, scaleVec, subVec, withinDistance )

-- Mọi phép so sánh khoảng cách trong module này dùng bình phương khoảng cách
-- (không sqrt). Thứ tự gần/xa và kết quả <=, < không đổi vì khoảng cách >= 0.

-- Bình phương của một range (range âm coi như 0).
rangeSq :: Float -> Float
rangeSq range = r * r
  where
    r = max 0 range
{-# INLINE rangeSq #-}

-- Chọn mục tiêu gần nhất, giữ nguyên toàn bộ giá trị (ví dụ gồm cả ID).
-- Danh sách đầu vào phải được lọc trước: chỉ truyền người chơi còn sống/kết nối.
-- Một lượt duyệt, mỗi phần tử tính khoảng cách đúng một lần; hòa thì lấy phần tử đầu.
nearestBy :: (a -> Vec2) -> Vec2 -> [a] -> Maybe a
nearestBy _ _ [] = Nothing
nearestBy getPos origin (first:rest) = Just (go first (distTo first) rest)
  where
    distTo c = distanceSq origin (getPos c)

    go !best !_ [] = best
    go !best !bestD (c:cs)
      | d < bestD = go c d cs
      | otherwise = go best bestD cs
      where
        d = distTo c

-- Chỉ xét mục tiêu nằm trong phạm vi; trả Nothing nếu không có mục tiêu phù hợp.
-- Lọc và chọn gần nhất trong cùng một lượt duyệt (không tạo danh sách trung gian).
nearestWithinRange :: (a -> Vec2) -> Vec2 -> Float -> [a] -> Maybe a
nearestWithinRange getPos origin range = go Nothing
  where
    limitSq = rangeSq range

    go !acc [] = fmap fst acc
    go !acc (c:cs)
      | d > limitSq = go acc cs
      | otherwise = case acc of
          Just (_, bestD) | bestD <= d -> go acc cs
          _                            -> go (Just (c, d)) cs
      where
        d = distanceSq origin (getPos c)

-- Giữ mục tiêu hiện tại nếu vẫn còn trong aggro range; nếu không thì chọn mục tiêu
-- gần nhất trong range. getId giúp giữ đúng player thay vì chỉ giữ vị trí của họ.
retainTarget
  :: (a -> Int) -> Int -> (a -> Vec2) -> Vec2 -> Float -> [a] -> Maybe a
retainTarget getId currentId getPos origin aggroRange candidates =
  case find isCurrent candidates of
    Just target | withinDistance aggroRange origin (getPos target) -> Just target
    _ -> nearestWithinRange getPos origin aggroRange candidates
  where
    isCurrent target = getId target == currentId

directionToTarget :: Vec2 -> Vec2 -> Vec2
directionToTarget fromPos targetPos = normalize (subVec targetPos fromPos)

-- Khi hai entity chồng (gần) đúng một vị trí, chọn hướng dự phòng ổn định (1,0)
-- để tránh vector 0/NaN. Kiểm tra trên kết quả normalize nên cũng bắt được
-- trường hợp delta cực nhỏ nhưng khác 0.
directionAwayFromTarget :: Vec2 -> Vec2 -> Vec2
directionAwayFromTarget fromPos targetPos =
  case normalize (subVec fromPos targetPos) of
    (0, 0) -> (1, 0)
    dir    -> dir

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

-- AI cận chiến: Nothing nghĩa là chưa có mục tiêu hợp lệ.
-- Chỉ tính khoảng cách một lần cho cả hai ngưỡng aggro/attack.
decideAIState :: Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideAIState _ _ Nothing _ = Idle
decideAIState aggroRange attackRange (Just targetPos) monsterPos
  | dSq > rangeSq aggroRange  = Idle
  | dSq <= rangeSq attackRange = Attacking
  | otherwise                  = Chasing
  where
    dSq = distanceSq monsterPos targetPos

-- Quái tầm xa giữ khoảng cách: quá gần thì lùi, quá xa thì tiến, còn lại đứng bắn.
rangedMove :: Float -> Float -> Vec2 -> Vec2 -> RangedMove
rangedMove minimumRange preferredRange monsterPos targetPos
  | dSq < rangeSq minRange = Retreat
  | dSq > rangeSq maxRange = Approach
  | otherwise              = Hold
  where
    dSq      = distanceSq monsterPos targetPos
    minRange = max 0 minimumRange
    maxRange = max minRange preferredRange

decideRangedState
  :: Float -> Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideRangedState _ _ _ Nothing _ = Idle
decideRangedState aggroRange minimumRange preferredRange (Just targetPos) monsterPos
  | not (shouldChase aggroRange monsterPos targetPos) = Idle
  | otherwise = case rangedMove minimumRange preferredRange monsterPos targetPos of
      Retreat  -> KeepingDistance
      Approach -> Chasing
      Hold     -> Attacking

-- Hướng cơ bản theo state; GameLoop cần nhân với tốc độ và dt.
moveDirectionFor :: AIState -> Vec2 -> Vec2 -> Vec2
moveDirectionFor state monsterPos targetPos = case state of
  Chasing         -> directionToTarget monsterPos targetPos
  KeepingDistance -> directionAwayFromTarget monsterPos targetPos
  _               -> (0, 0)

-- Giống moveDirectionFor nhưng dừng khi đã tới khoảng cách mong muốn.
-- Dùng cho melee để không chạy xuyên qua người chơi khi còn cách attackRange.
moveDirectionWithRange :: AIState -> Float -> Vec2 -> Vec2 -> Vec2
moveDirectionWithRange state desiredRange monsterPos targetPos =
  case state of
    Chasing
      | dSq > stopSq -> directionToTarget monsterPos targetPos
      | otherwise    -> (0, 0)
    KeepingDistance
      | dSq < stopSq -> directionAwayFromTarget monsterPos targetPos
      | otherwise    -> (0, 0)
    _ -> (0, 0)
  where
    dSq    = distanceSq monsterPos targetPos
    stopSq = rangeSq desiredRange

-- Ước lượng vị trí mục tiêu khi đạn cần thời gian bay tới.
-- predictionSeconds thường nên bị giới hạn nhỏ ở phía gọi để tránh dự đoán quá xa.
predictTargetPosition :: Vec2 -> Vec2 -> Float -> Vec2
predictTargetPosition targetPos targetVelocity predictionSeconds =
  addVec targetPos (scaleVec (max 0 predictionSeconds) targetVelocity)

-- Tuần tra về waypoint; đứng yên khi đã tới đủ gần.
patrolDirection :: Vec2 -> Vec2 -> Float -> Vec2
patrolDirection currentPos waypoint stopDistance
  | withinDistance stopDistance currentPos waypoint = (0, 0)
  | otherwise = directionToTarget currentPos waypoint