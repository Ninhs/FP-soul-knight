module Animation
  ( AnimKind(..)
  , AnimState(..)
  , SpriteSet(..)
  , startAnim
  , setAnimKind
  , stepAnim
  , animFinished
  , currentFrame
  ) where

import Graphics.Gloss (Picture(..))

data AnimKind = Idle | Walk | Attack | Die
  deriving (Eq, Show)

-- Bộ sprite: 2 hero (soldier, archer), quái thường (mon, orc), boss (demon)
data SpriteSet = SoldierSet | ArcherSet | MonsterSet | OrcSet | DemonSet
  deriving (Eq, Show)

data AnimState = AnimState
  { animKind :: AnimKind
  , animTime :: Float
  } deriving (Eq, Show)

startAnim :: AnimKind -> AnimState
startAnim kind = AnimState kind 0

-- Chỉ reset khi trạng thái thực sự đổi
setAnimKind :: AnimKind -> AnimState -> AnimState
setAnimKind kind st
  | animKind st == kind = st
  | otherwise = startAnim kind

stepAnim :: Float -> AnimState -> AnimState
stepAnim dt st = st { animTime = animTime st + max 0 dt }

-- (fps, có lặp không)
animSpec :: AnimKind -> (Float, Bool)
animSpec Idle   = (3, True)
animSpec Walk   = (5, True)
animSpec Attack = (8, False)
animSpec Die    = (5, False)

frameIndex :: Int -> AnimState -> Int
frameIndex n st
  | n <= 0 = 0
  | looping = rawIndex `mod` n
  | otherwise = min (n - 1) rawIndex
  where
    (fps, looping) = animSpec (animKind st)
    rawIndex = floor (animTime st * fps)

currentFrame :: [Picture] -> AnimState -> Picture
currentFrame [] _ = Blank
currentFrame frames st = frames !! frameIndex (length frames) st

-- Đã phát hết một lượt (chỉ có ý nghĩa với animation không lặp)
animFinished :: [Picture] -> AnimState -> Bool
animFinished frames st =
  not looping && floor (animTime st * fps) >= length frames
  where
    (fps, looping) = animSpec (animKind st)