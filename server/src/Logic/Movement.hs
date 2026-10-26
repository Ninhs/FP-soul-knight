
{-# LANGUAGE BangPatterns #-}

module Logic.Movement
  ( Vec2
  , addVec
  , subVec
  , scaleVec
  , magnitude
  , magnitudeSq
  , normalize
  , clampMagnitude
  , moveTowards
  , distance
  , distanceSq
  , withinDistance
  , stepFromInput
  , moveWithAxisCollision
  , moveWithCollisionSteps
  , moveAndSlide
  , circleIntersectsAABB
  , blockedByTiles
  , movePlayerWithCollision
  ) where

import Types.GameState (Player(..), PlayerStatus(..), moveSpeed)

-- Tọa độ thế giới (x, y).
type Vec2 = (Float, Float)

epsilon, epsilonSq :: Float
epsilon = 0.00001
epsilonSq = epsilon * epsilon

isFinite :: Float -> Bool
isFinite value = not (isNaN value || isInfinite value)

addVec :: Vec2 -> Vec2 -> Vec2
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVec :: Vec2 -> Vec2 -> Vec2
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

scaleVec :: Float -> Vec2 -> Vec2
scaleVec k (x, y) = (k * x, k * y)

magnitudeSq :: Vec2 -> Float
magnitudeSq (x, y) = x * x + y * y

magnitude :: Vec2 -> Float
magnitude = sqrt . magnitudeSq

normalize :: Vec2 -> Vec2
normalize vector@(x, y)
  | lenSq <= epsilonSq = (0, 0)
  | otherwise =
      let inv = 1 / sqrt lenSq
      in (x * inv, y * inv)
  where
    lenSq = magnitudeSq vector

clampMagnitude :: Float -> Vec2 -> Vec2
clampMagnitude maxLen vector
  | maxLen <= 0 = (0, 0)
  | lenSq <= maxLen * maxLen = vector
  | lenSq <= epsilonSq = (0, 0)
  | otherwise = scaleVec (maxLen / sqrt lenSq) vector
  where
    lenSq = magnitudeSq vector

distanceSq :: Vec2 -> Vec2 -> Float
distanceSq a b = magnitudeSq (subVec b a)

distance :: Vec2 -> Vec2 -> Float
distance a b = sqrt (distanceSq a b)

withinDistance :: Float -> Vec2 -> Vec2 -> Bool
withinDistance range a b =
  distanceSq a b <= r * r
  where
    r = max 0 range

moveTowards :: Vec2 -> Vec2 -> Float -> Vec2
moveTowards current target maxStep
  | lenSq <= stepSq || lenSq <= epsilonSq = target
  | otherwise = addVec current (scaleVec (step / sqrt lenSq) delta)
  where
    delta = subVec target current
    lenSq = magnitudeSq delta
    step = max 0 maxStep
    stepSq = step * step

-- Chuẩn hóa input để đi chéo không nhanh hơn đi thẳng.
stepFromInput :: Vec2 -> Float -> Float -> Vec2
stepFromInput inputDir speed dt =
  scaleVec (max 0 speed * max 0 dt) (normalize inputDir)

-- Tách trục X/Y để entity trượt dọc tường.
-- isBlocked phải kiểm tra hitbox đầy đủ của entity.
moveWithAxisCollision :: (Vec2 -> Bool) -> Vec2 -> Vec2 -> Vec2
moveWithAxisCollision isBlocked (x, y) (dx, dy) =
  let x1
        | dx == 0 = x
        | isBlocked (x + dx, y) = x
        | otherwise = x + dx

      y1
        | dy == 0 = y
        | isBlocked (x1, y + dy) = y
        | otherwise = y + dy
  in (x1, y1)

maxSubSteps :: Int
maxSubSteps = 256

-- Chia chuyển động thành các bước nhỏ để giảm xuyên tường.
moveWithCollisionSteps
  :: (Vec2 -> Bool) -> Float -> Vec2 -> Vec2 -> Vec2
moveWithCollisionSteps isBlocked requestedStep start (dx0, dy0)
  | not (isFinite dx0 && isFinite dy0) = start
  | dx0 == 0 && dy0 == 0 = start
  | otherwise = walk steps start
  where
    stepLimit
      | not (isFinite requestedStep) || requestedStep <= 0 = 0.25
      | otherwise = max 0.01 requestedStep

    maxDelta = stepLimit * fromIntegral maxSubSteps
    delta = clampMagnitude maxDelta (dx0, dy0)
    (dx, dy) = delta

    rawSteps = max (abs dx) (abs dy) / stepLimit
    steps = max 1 (min maxSubSteps (ceiling rawSteps))
    smallDelta = scaleVec (1 / fromIntegral steps) delta

    walk :: Int -> Vec2 -> Vec2
    walk !n !pos
      | n <= 0 = pos
      | next == pos = pos
      | otherwise = walk (n - 1) next
      where
        next = moveWithAxisCollision isBlocked pos smallDelta

-- Hitbox tròn với ô tường hình chữ nhật AABB.
circleIntersectsAABB :: Vec2 -> Float -> Vec2 -> Vec2 -> Bool
circleIntersectsAABB (cx, cy) radius (minX, minY) (maxX, maxY) =
  let nearestX = max minX (min cx maxX)
      nearestY = max minY (min cy maxY)
      dx = cx - nearestX
      dy = cy - nearestY
      r = max 0 radius
  in dx * dx + dy * dy <= r * r

-- Mỗi tường được biểu diễn bằng ((minX,minY),(maxX,maxY)).
blockedByTiles :: Float -> [(Vec2, Vec2)] -> Vec2 -> Bool
blockedByTiles radius walls pos =
  any blocked walls
  where
    blocked (tileMin, tileMax) =
      circleIntersectsAABB pos radius tileMin tileMax

moveAndSlide :: (Vec2 -> Bool) -> Float -> Vec2 -> Vec2 -> Vec2
moveAndSlide = moveWithCollisionSteps

-- Di chuyển Player ở phía server.
-- isBlocked phải sử dụng bán kính nhân vật để kiểm tra va chạm.
movePlayerWithCollision
  :: (Vec2 -> Bool) -> Float -> Vec2 -> Player -> Player
movePlayerWithCollision isBlocked dt input player
  | status player /= Alive = player
  | otherwise =
      player
        { position =
            moveWithCollisionSteps
              isBlocked
              0.2
              (position player)
              delta
        }
  where
    elapsed = max 0 (min 0.25 dt)
    delta = stepFromInput input (moveSpeed (stats player)) elapsed
