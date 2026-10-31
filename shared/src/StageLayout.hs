-- Bố cục "một phòng một màn" (kiểu Soul Knight): Màn 0 phòng đầu (không quái),
-- màn 1 phòng quái nhỏ, màn 2 phòng boss. Dùng chung cho server và client.
-- Toạ độ pixel, gốc ở TÂM PHÒNG, trục y hướng lên, ô = 32 px.
module StageLayout
  ( stageCount
  , lastStage
  , roomCols, roomRows, roomWidth, roomHeight
  , floorMinX, floorMaxX, floorMinY, floorMaxY
  , doorCenter
  , exitZoneRect
  , exitZoneCenter
  , blockedByWalls
  , inExitZone
  , entrySpawn
  , StageSpawn(..)
  , stageSpawns
  ) where

import qualified MapLayout as Map
import Types.GameState (AttackType(..), BodyType(..), MonsterKind(..))

-- Số màn: 0 = phòng đầu, 1 = phòng quái nhỏ, 2 = phòng boss
stageCount, lastStage :: Int
stageCount = 3
lastStage = stageCount - 1

roomCols, roomRows :: Int
roomCols = 36
roomRows = 20

roomWidth, roomHeight :: Float
roomWidth = fromIntegral roomCols * Map.tileSize     -- 1152
roomHeight = fromIntegral roomRows * Map.tileSize    -- 640

-- Vùng sàn đi được (2 hàng trên cùng là tường + mặt tường, hàng dưới cùng là tường)
floorMinX, floorMaxX, floorMinY, floorMaxY :: Float
floorMinX = negate roomWidth / 2 + Map.tileSize
floorMaxX = roomWidth / 2 - Map.tileSize
floorMinY = negate roomHeight / 2 + Map.tileSize
floorMaxY = roomHeight / 2 - 2 * Map.tileSize

-- Cửa sắt nằm giữa tường trên (rộng 2 ô, cao 2 ô)
doorCenter :: (Float, Float)
doorCenter = (0, roomHeight / 2 - Map.tileSize)

-- Ô trước cửa: dài 1 ô, rộng 3 ô, ngay dưới cửa. Chạm vào để qua màn.
exitZoneRect :: Map.Rect
exitZoneRect = Map.Rect (negate (1.5 * Map.tileSize)) (floorMaxY - Map.tileSize)
                        (3 * Map.tileSize) Map.tileSize

exitZoneCenter :: (Float, Float)
exitZoneCenter =
  ( Map.rectX exitZoneRect + Map.rectWidth exitZoneRect / 2
  , Map.rectY exitZoneRect + Map.rectHeight exitZoneRect / 2 )

-- Tâm một hình tròn bán kính r có đụng tường không (ra ngoài vùng sàn).
blockedByWalls :: Float -> (Float, Float) -> Bool
blockedByWalls r (x, y) =
  x < floorMinX + r || x > floorMaxX - r || y < floorMinY + r || y > floorMaxY - r

-- Hình tròn bán kính r có chạm ô trước cửa không.
inExitZone :: Float -> (Float, Float) -> Bool
inExitZone r pos = Map.circleHitsRect r pos exitZoneRect

-- Chỗ đứng của người chơi thứ i khi vào màn (hàng dưới cùng, đi vào từ phía dưới).
entrySpawn :: Int -> (Float, Float)
entrySpawn i = (negate 60 + fromIntegral i * 40, floorMinY + 48)

data StageSpawn = StageSpawn
  { ssPos        :: (Float, Float)
  , ssKind       :: MonsterKind
  , ssHp         :: Int
  , ssDamage     :: Int
  , ssAttackRate :: Float
  }

-- Quái của từng màn (màn 0 không có quái).
stageSpawns :: Int -> [StageSpawn]
stageSpawns 1 =
  [ StageSpawn (-360, 140) (Regular Melee GlassBody) 50 8 0.9
  , StageSpawn ( 360, 140) (Regular Melee TankBody)  75 6 1.2
  , StageSpawn (   0,  40) (Regular Melee GlassBody) 50 8 0.9
  , StageSpawn (-420, -80) (Regular Ranged GlassBody) 42 7 1.1
  , StageSpawn ( 420, -80) (Regular Ranged TankBody)  60 5 1.3
  , StageSpawn (-180, 160) (Regular Melee TankBody)  80 6 1.2
  ]
stageSpawns 2 = [StageSpawn (0, 20) Boss 600 18 1.0]
stageSpawns _ = []
