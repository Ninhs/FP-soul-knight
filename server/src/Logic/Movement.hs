module Logic.Movement
  ( Vec2
  , addVec
  , subVec
  , scaleVec
  , magnitude
  , normalize
  , moveTowards
  , distance
  , stepFromInput
  , moveWithAxisCollision
  ) where

type Vec2 = (Float, Float)

addVec :: Vec2 -> Vec2 -> Vec2
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVec :: Vec2 -> Vec2 -> Vec2
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

scaleVec :: Float -> Vec2 -> Vec2
scaleVec k (x, y) = (k * x, k * y)

magnitude :: Vec2 -> Float
magnitude (x, y) = sqrt (x * x + y * y)

normalize :: Vec2 -> Vec2
normalize v =
  let len = magnitude v
  in if len <= 0.00001 then (0, 0) else scaleVec (1 / len) v

distance :: Vec2 -> Vec2 -> Float
distance a b = magnitude (subVec b a)

-- Trả về vị trí mới khi di chuyển về phía target tối đa maxStep.
moveTowards :: Vec2 -> Vec2 -> Float -> Vec2
moveTowards current target maxStep =
  let delta = subVec target current
      len = magnitude delta
      step = max 0 maxStep
  in if len <= step || len <= 0.00001
       then target
       else addVec current (scaleVec (step / len) delta)

-- Độ dời trong một tick từ input của client.
-- Luôn chuẩn hóa hướng nên đi chéo không nhanh hơn, và client gửi
-- vector lớn (ví dụ (100, 0)) cũng không chạy nhanh hơn tốc độ thật.
-- speed: đơn vị/giây, dt: giây của tick.
stepFromInput :: Vec2 -> Float -> Float -> Vec2
stepFromInput inputDir speed dt =
  scaleVec (max 0 speed * max 0 dt) (normalize inputDir)

-- isBlocked trả True khi vị trí ứng viên bị chặn (tường).
-- Thử trục X rồi trục Y riêng nên nhân vật trượt dọc tường.
-- Lưu ý: độ dời mỗi tick phải nhỏ hơn độ dày tường (khoảng một ô tile).
moveWithAxisCollision :: (Vec2 -> Bool) -> Vec2 -> Vec2 -> Vec2
moveWithAxisCollision isBlocked (x, y) (dx, dy) =
  let candidateX = (x + dx, y)
      afterX = if isBlocked candidateX then (x, y) else candidateX
      (x1, y1) = afterX
      candidateY = (x1, y1 + dy)
  in if isBlocked candidateY then afterX else candidateY
