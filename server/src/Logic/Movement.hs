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
  ) where

-- Tọa độ thế giới: (x, y). Các hàm trong module này dùng cùng đơn vị.
type Vec2 = (Float, Float)

-- Tạo Vec2 với hai thành phần đã được tính sẵn (strict), tránh tích tụ thunk
-- khi vị trí được cập nhật mỗi tick và lưu lâu trong GameState.
mkVec :: Float -> Float -> Vec2
mkVec !x !y = (x, y)
{-# INLINE mkVec #-}

epsilon, epsilonSq :: Float
epsilon   = 0.00001
epsilonSq = epsilon * epsilon

isFinite :: Float -> Bool
isFinite v = not (isNaN v || isInfinite v)
{-# INLINE isFinite #-}

addVec :: Vec2 -> Vec2 -> Vec2
addVec (x1, y1) (x2, y2) = mkVec (x1 + x2) (y1 + y2)
{-# INLINE addVec #-}

subVec :: Vec2 -> Vec2 -> Vec2
subVec (x1, y1) (x2, y2) = mkVec (x1 - x2) (y1 - y2)
{-# INLINE subVec #-}

scaleVec :: Float -> Vec2 -> Vec2
scaleVec k (x, y) = mkVec (k * x) (k * y)
{-# INLINE scaleVec #-}

-- Bình phương độ dài: không tốn sqrt, dùng để so sánh khoảng cách.
magnitudeSq :: Vec2 -> Float
magnitudeSq (x, y) = x * x + y * y
{-# INLINE magnitudeSq #-}

magnitude :: Vec2 -> Float
magnitude = sqrt . magnitudeSq
{-# INLINE magnitude #-}

-- Vector gần bằng 0 trả về (0,0), tránh chia cho 0 và NaN.
-- Chỉ tính sqrt một lần và nhân với nghịch đảo thay vì chia hai lần.
normalize :: Vec2 -> Vec2
normalize v@(x, y)
  | lenSq <= epsilonSq = (0, 0)
  | otherwise          = let !inv = 1 / sqrt lenSq in mkVec (x * inv) (y * inv)
  where
    lenSq = magnitudeSq v

-- Giới hạn độ dài vector, hữu ích để giữ tốc độ di chuyển không vượt mức.
-- So sánh bằng bình phương nên trường hợp "chưa vượt" không cần sqrt.
clampMagnitude :: Float -> Vec2 -> Vec2
clampMagnitude maxLen v
  | maxLen <= 0                = (0, 0)
  | lenSq <= maxLen * maxLen   = v
  | lenSq <= epsilonSq         = (0, 0)
  | otherwise                  = scaleVec (maxLen / sqrt lenSq) v
  where
    lenSq = magnitudeSq v

-- Bình phương khoảng cách giữa hai điểm.
distanceSq :: Vec2 -> Vec2 -> Float
distanceSq a b = magnitudeSq (subVec b a)
{-# INLINE distanceSq #-}

distance :: Vec2 -> Vec2 -> Float
distance a b = sqrt (distanceSq a b)
{-# INLINE distance #-}

-- Không dùng sqrt: so sánh bình phương khoảng cách với bình phương range.
withinDistance :: Float -> Vec2 -> Vec2 -> Bool
withinDistance range a b = distanceSq a b <= r * r
  where
    r = max 0 range
{-# INLINE withinDistance #-}

-- Đi về phía target tối đa maxStep; không vượt qua target.
moveTowards :: Vec2 -> Vec2 -> Float -> Vec2
moveTowards current target maxStep
  | lenSq <= stepSq || lenSq <= epsilonSq = target
  | otherwise = addVec current (scaleVec (step / sqrt lenSq) delta)
  where
    delta  = subVec target current
    lenSq  = magnitudeSq delta
    step   = max 0 maxStep
    stepSq = step * step

-- Độ dời của input trong dt giây. Chuẩn hóa input để đi chéo không nhanh hơn.
stepFromInput :: Vec2 -> Float -> Float -> Vec2
stepFromInput inputDir speed dt =
  scaleVec (max 0 speed * max 0 dt) (normalize inputDir)

-- Kiểm tra va chạm theo từng trục, cho phép trượt dọc tường.
-- isBlocked nên kiểm tra cả hitbox của entity, không chỉ tâm tọa độ.
-- Trục nào không có độ dời thì bỏ qua luôn lời gọi isBlocked (hàm tốn kém nhất);
-- kết quả vẫn y hệt vì vị trí trục đó không đổi.
moveWithAxisCollision :: (Vec2 -> Bool) -> Vec2 -> Vec2 -> Vec2
moveWithAxisCollision isBlocked (x, y) (dx, dy) =
  let !x1 = if dx == 0 then x
            else if isBlocked (x + dx, y) then x
            else x + dx
      !y1 = if dy == 0 then y
            else if isBlocked (x1, y + dy) then y
            else y + dy
  in (x1, y1)

-- Giới hạn số bước phụ mỗi lần gọi để dt/tốc độ bất thường không làm treo server.
maxSubSteps :: Int
maxSubSteps = 64

-- Di chuyển có chia nhỏ bước để hạn chế xuyên tường khi dt lớn hoặc tốc độ cao.
-- maxStep là khoảng cách tối đa mỗi bước phụ (ví dụ 0.2 - 0.4 world units).
-- Nếu maxStep <= 0 (hoặc NaN), dùng giá trị an toàn mặc định 0.25.
-- Delta NaN/Infinity -> đứng yên (tránh ceiling NaN ra số khổng lồ làm treo vòng lặp).
-- Dừng sớm khi một bước phụ không làm vị trí thay đổi (kẹt tường cả hai trục):
-- các bước sau chắc chắn cho cùng kết quả vì isBlocked là hàm thuần.
moveWithCollisionSteps
  :: (Vec2 -> Bool) -> Float -> Vec2 -> Vec2 -> Vec2
moveWithCollisionSteps isBlocked requestedStep start delta@(dx, dy)
  | not (isFinite dx && isFinite dy) = start
  | dx == 0 && dy == 0               = start
  | otherwise                        = walk steps start
  where
    stepLimit
      | isNaN requestedStep || requestedStep <= 0 = 0.25
      | otherwise                                 = requestedStep

    rawSteps = max (abs dx) (abs dy) / stepLimit
    steps :: Int
    steps
      | rawSteps >= fromIntegral maxSubSteps = maxSubSteps
      | otherwise                            = max 1 (ceiling rawSteps)

    smallDelta = scaleVec (1 / fromIntegral steps) delta

    walk :: Int -> Vec2 -> Vec2
    walk !n !pos
      | n <= 0    = pos
      | next == pos = pos
      | otherwise = walk (n - 1) next
      where
        next = moveWithAxisCollision isBlocked pos smallDelta

-- Alias diễn đạt rõ ý: di chuyển rồi trượt dọc vật cản.
moveAndSlide :: (Vec2 -> Bool) -> Float -> Vec2 -> Vec2 -> Vec2
moveAndSlide = moveWithCollisionSteps
{-# INLINE moveAndSlide #-}