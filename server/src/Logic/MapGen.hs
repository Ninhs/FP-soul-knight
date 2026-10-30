{-# LANGUAGE ScopedTypeVariables #-}
-- Sinh MapState ban đầu cho một ván (dùng chung bố cục map với client qua MapLayout).
module Logic.MapGen
  ( roomMapSize
  , newMapState
  ) where

import qualified MapLayout as Map
import Types.GameState

-- Server hiện chỉ chơi bản đồ nhỏ (SmallMap) để khớp client.
roomMapSize :: Map.MapSize
roomMapSize = Map.SmallMap

-- Chỉ số cơ bản theo nghề.
statsFor :: Role -> Stats
statsFor Warrior = Stats { hp = 150, maxHp = 150, damage = 25, moveSpeed = 4.5, atkSpeed = 0.45, atkRange = 3.0 }
statsFor Archer  = Stats { hp = 100, maxHp = 100, damage = 18, moveSpeed = 5.0, atkSpeed = 0.45, atkRange = 8.0 }

-- Tạo ván mới cho danh sách người chơi (id, hồ sơ) đang ở phòng.
newMapState :: [(Int, PlayerProfile)] -> MapState
newMapState profs = MapState
  { mapIndex  = 1
  , isBossMap = False
  , players   = zipWith mkPlayer [0 ..] profs
  , monsters  = concatMap mkRoomMonsters (Map.monsterSpawns size)
  , bullets   = []
  , drops     = []
  , portal    = Portal False (Map.toWorldPosition (Map.roomCenter size Map.BossRoom))
  }
  where
    size = roomMapSize
    (sx, sy) = Map.toWorldPosition (Map.startPosition size)

    mkPlayer :: Int -> (Int, PlayerProfile) -> Player
    mkPlayer i (pid, prof) = Player
      { playerId    = pid
      , position    = (sx + fromIntegral i * 1.2, sy)   -- đứng cạnh nhau, không chồng lên
      , stats       = statsFor (profileRole prof)
      , status      = Alive
      , pendingExp  = 0
      , pendingGold = 0
      }

    mkRoomMonsters (room, spawns) = zipWith (mkMonster room) [1 ..] spawns

    mkMonster room (idx :: Int) sp =
      let mid = case room of
            Map.NormalA  -> 100 + idx
            Map.NormalB  -> 200 + idx
            Map.BossRoom -> 900
            Map.StartRoom -> 0
          (fx, fy) = Map.spawnFrac sp
          (cx, cy) = Map.roomCenter size room
          (hw, hh) = Map.roomHalfSize size room
      in Monster
           { monsterId = mid
           , mPosition = Map.toWorldPosition (cx + fx * hw, cy + fy * hh)
           , mStats    = Stats
               { hp = Map.spawnHp sp, maxHp = Map.spawnHp sp
               , damage = Map.spawnDamage sp
               , moveSpeed = 68 / Map.worldScale
               , atkSpeed = Map.spawnAttackRate sp
               , atkRange = 3.0 }
           , kind      = Map.spawnKind sp
           , mCooldown = 0
           }
