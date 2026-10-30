module MapLayout
  ( MapSize(..)
  , mapDimensions
  , RoomId(..)
  , Link(..)
  , Rect(..)
  , MonsterSpawn(..)
  , tileSize
  , wallThickness
  , playerHitRadius
  , monsterHitRadius
  , spawnMargin
  , spawnFracLimit
  , mapWidth
  , mapHeight
  , roomRect
  , roomCenter
  , roomHalfSize
  , startPosition
  , permanentWalls
  , linkDoor
  , closedDoorRects
  , solidRects
  , openLinks
  , circleHitsRect
  , isBlocked
  , geomRoomContains
  , roomAt
  , homeRoomOfMonster
  , monsterSpawns
  ) where

import qualified Types.GameState as GS

data MapSize = SmallMap | MediumMap | LargeMap
  deriving (Eq, Show, Read, Enum, Bounded)

data RoomId = StartRoom | NormalA | NormalB | BossRoom
  deriving (Eq, Show, Read, Enum, Bounded)

data Link = LinkBLtoTL | LinkTLtoTR | LinkTRtoBR
  deriving (Eq, Show, Read, Enum, Bounded)

data Rect = Rect
  { rectX :: Float
  , rectY :: Float
  , rectWidth :: Float
  , rectHeight :: Float
  } deriving (Eq, Show)

data MonsterSpawn = MonsterSpawn
  { spawnFrac :: (Float, Float)
  , spawnKind :: GS.MonsterKind
  , spawnHp :: Int
  , spawnDamage :: Int
  , spawnAttackRate :: Float
  } deriving (Eq, Show)

tileSize, wallThickness :: Float
tileSize = 32
wallThickness = 24

playerHitRadius, monsterHitRadius, spawnMargin, spawnFracLimit :: Float
playerHitRadius = 16
monsterHitRadius = 24
spawnMargin = 48
spawnFracLimit = 0.7

mapDimensions :: MapSize -> (Int, Int)
mapDimensions SmallMap = (40, 24)
mapDimensions MediumMap = (56, 32)
mapDimensions LargeMap = (128, 72)

mapWidth, mapHeight :: MapSize -> Float
mapWidth size = fromIntegral (fst (mapDimensions size)) * tileSize
mapHeight size = fromIntegral (snd (mapDimensions size)) * tileSize

roomRect :: MapSize -> RoomId -> Rect
roomRect size room = case room of
  StartRoom -> Rect (negate halfW) (negate halfH) halfW halfH
  NormalA -> Rect (negate halfW) 0 halfW halfH
  NormalB -> Rect 0 0 halfW halfH
  BossRoom -> Rect 0 (negate halfH) halfW halfH
  where
    halfW = mapWidth size / 2
    halfH = mapHeight size / 2

roomCenter :: MapSize -> RoomId -> (Float, Float)
roomCenter size room =
  let bounds = roomRect size room
  in (rectX bounds + rectWidth bounds / 2,
      rectY bounds + rectHeight bounds / 2)

roomHalfSize :: MapSize -> RoomId -> (Float, Float)
roomHalfSize size room =
  let bounds = roomRect size room
  in (rectWidth bounds / 2, rectHeight bounds / 2)

startPosition :: MapSize -> (Float, Float)
startPosition size = roomCenter size StartRoom

permanentWalls :: MapSize -> [Rect]
permanentWalls size =
  [ box (negate hw) (negate hw + t) (negate hh) hh
  , box (hw - t) hw (negate hh) hh
  , box (negate hw + t) (hw - t) (negate hh) (negate hh + t)
  , box (negate hw + t) (hw - t) (hh - t) hh
  , box (negate hw + t) (negate (0.5771 * hw)) (bandLo size) (bandHi size)
  , box (negate (0.4072 * hw)) (0.4170 * hw) (bandLo size) (bandHi size)
  , box (0.5874 * hw) (hw - t) (bandLo size) (bandHi size)
  , box (negate (0.0435 * hw)) (0.0581 * hw)
      (0.5513 * hh) (hh - t)
  , box (negate (0.0483 * hw)) (0.0493 * hw)
      (negate hh + t) (bandLo size)
  ]
  where
    hw = mapWidth size / 2
    hh = mapHeight size / 2
    t = wallThickness

box :: Float -> Float -> Float -> Float -> Rect
box x0 x1 y0 y1 = Rect x0 y0 (x1 - x0) (y1 - y0)

bandLo, bandHi :: MapSize -> Float
bandLo size = -0.0347 * (mapHeight size / 2)
bandHi size = 0.0434 * (mapHeight size / 2)

linkDoor :: MapSize -> Link -> Rect
linkDoor size link = case link of
  LinkBLtoTL ->
    box (negate (0.5771 * hw)) (negate (0.4072 * hw)) (bandLo size) (bandHi size)
  LinkTLtoTR ->
    box (negate (0.0435 * hw)) (0.0581 * hw) (bandHi size) (0.5513 * hh)
  LinkTRtoBR ->
    box (0.4170 * hw) (0.5874 * hw) (bandLo size) (bandHi size)
  where
    hw = mapWidth size / 2
    hh = mapHeight size / 2

openLinks :: [RoomId] -> [Link]
openLinks cleared =
  [LinkBLtoTL]
    ++ [LinkTLtoTR | NormalA `elem` cleared]
    ++ [LinkTRtoBR | NormalB `elem` cleared]

closedDoorRects :: MapSize -> [Link] -> [Rect]
closedDoorRects size opened =
  [linkDoor size link | link <- [LinkBLtoTL, LinkTLtoTR, LinkTRtoBR],
    link `notElem` opened]

solidRects :: MapSize -> [Link] -> [Rect]
solidRects size opened = permanentWalls size ++ closedDoorRects size opened

circleHitsRect :: Float -> (Float, Float) -> Rect -> Bool
circleHitsRect radius (x, y) bounds =
  let nearestX = clamp (rectX bounds) (rectX bounds + rectWidth bounds) x
      nearestY = clamp (rectY bounds) (rectY bounds + rectHeight bounds) y
      dx = x - nearestX
      dy = y - nearestY
  in dx * dx + dy * dy <= max 0 radius * max 0 radius
  where
    clamp lower upper = max lower . min upper

isBlocked :: MapSize -> [Link] -> Float -> (Float, Float) -> Bool
isBlocked size opened radius position =
  any (circleHitsRect radius position) (solidRects size opened)

geomRoomContains :: MapSize -> RoomId -> Float -> (Float, Float) -> Bool
geomRoomContains size room radius (x, y) =
  x >= rectX bounds + inset
    && x <= rectX bounds + rectWidth bounds - inset
    && y >= rectY bounds + inset
    && y <= rectY bounds + rectHeight bounds - inset
  where
    bounds = roomRect size room
    inset = max 0 radius

roomAt :: MapSize -> (Float, Float) -> Maybe RoomId
roomAt size position
  | any (circleHitsRect 0 position) (permanentWalls size) = Nothing
  | y >= 0 = Just (if x >= 0 then NormalB else NormalA)
  | x >= 0 = Just BossRoom
  | otherwise = Just StartRoom
  where
    (x, y) = position

homeRoomOfMonster :: Int -> Maybe RoomId
homeRoomOfMonster monsterId = case monsterId `div` 100 of
  1 -> Just NormalA
  2 -> Just NormalB
  9 -> Just BossRoom
  _ -> Nothing

monsterSpawns :: MapSize -> [(RoomId, [MonsterSpawn])]
monsterSpawns _ =
  [ (StartRoom, [])
  , (NormalA,
      [ regular (-0.55, 0.55) GS.Melee GS.GlassBody 50 8 0.9
      , regular (-0.4, 0.4) GS.Melee GS.TankBody 75 6 1.2
      , regular (0.25, 0.55) GS.Ranged GS.GlassBody 42 7 1.1
      , regular (0.4, -0.5) GS.Melee GS.GlassBody 50 8 0.9
      , regular (-0.2, 0.25) GS.Ranged GS.TankBody 60 5 1.3
      ])
  , (NormalB,
      [ regular (0.2, 0.4) GS.Melee GS.GlassBody 50 8 0.9
      , regular (0.4, 0.55) GS.Melee GS.TankBody 75 6 1.2
      , regular (0.55, 0.25) GS.Ranged GS.GlassBody 42 7 1.1
      , regular (0.3, 0.6) GS.Melee GS.GlassBody 50 8 0.9
      , regular (0.6, 0.45) GS.Ranged GS.TankBody 60 5 1.3
      , regular (0.45, 0.6) GS.Melee GS.TankBody 80 6 1.2
      ])
  , (BossRoom,
      [MonsterSpawn (0, 0) GS.Boss 600 18 1.0])
  ]
  where
    regular (fx, fy) attack body hp damage attackRate =
      MonsterSpawn (fx, fy) (GS.Regular attack body) hp damage attackRate
