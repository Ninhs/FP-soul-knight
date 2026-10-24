module Logic.Combat
  ( Vec2
  , circlesOverlap
  , projectileHitsTarget
  , damageAfterDefence
  , isDead
  , knockbackPosition
  , knockbackWithCollision
  , inFanAttack
  , inAreaAttack
  , isInvincibleAt
  , startInvincibility
  ) where

import Logic.Movement
  ( Vec2, addVec, distance, magnitude, moveWithAxisCollision
  , normalize, scaleVec, subVec )

circlesOverlap :: Vec2 -> Float -> Vec2 -> Float -> Bool
circlesOverlap posA radiusA posB radiusB =
  distance posA posB <= max 0 radiusA + max 0 radiusB

projectileHitsTarget :: Vec2 -> Float -> Vec2 -> Float -> Bool
projectileHitsTarget = circlesOverlap

damageAfterDefence :: Float -> Float -> Float
damageAfterDefence rawDamage defence =
  max 0 (rawDamage - max 0 defence)

isDead :: Float -> Bool
isDead hp = hp <= 0

-- Đẩy target ra xa source một đoạn amount (không kiểm tra tường).
knockbackPosition :: Vec2 -> Vec2 -> Float -> Vec2
knockbackPosition source target amount =
  addVec target (scaleVec (max 0 amount) (normalize (subVec target source)))

-- Giống knockbackPosition nhưng không đẩy xuyên tường.
knockbackWithCollision :: (Vec2 -> Bool) -> Vec2 -> Vec2 -> Float -> Vec2
knockbackWithCollision isBlocked source target amount =
  moveWithAxisCollision isBlocked target
    (scaleVec (max 0 amount) (normalize (subVec target source)))

-- Mục tiêu có nằm trong hình quạt không (đòn kiếm).
-- halfAngleDegrees: nửa góc mở của quạt, tính bằng độ.
inFanAttack :: Vec2 -> Vec2 -> Vec2 -> Float -> Float -> Bool
inFanAttack origin facing targetPos range halfAngleDegrees =
  let delta = subVec targetPos origin
      d = magnitude delta
      f = normalize facing
      t = normalize delta
      dot = fst f * fst t + snd f * snd t
      threshold = cos (halfAngleDegrees * pi / 180)
  in d <= max 0 range && d > 0.00001 && dot >= threshold

-- Nổ vùng (boom, phép).
inAreaAttack :: Vec2 -> Float -> Vec2 -> Float -> Bool
inAreaAttack = circlesOverlap

-- Đang bất tử khi thời điểm hiện tại còn nhỏ hơn invincibleUntil.
-- Cùng đơn vị thời gian với GameLoop (ví dụ giây).
isInvincibleAt :: Float -> Float -> Bool
isInvincibleAt now invincibleUntil = now < invincibleUntil

-- Thời điểm kết thúc bất tử sau khi bị trúng đòn (ví dụ duration = 0.5).
startInvincibility :: Float -> Float -> Float
startInvincibility now duration = now + max 0 duration
