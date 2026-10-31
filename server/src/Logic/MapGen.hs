{-# LANGUAGE ScopedTypeVariables #-}
-- Sinh MapState cho một ván: mỗi "màn" là một phòng riêng (xem StageLayout).
module Logic.MapGen
  ( roomMapSize
  , newMapState
  , enterStage
  ) where

import qualified MapLayout as Map
import qualified StageLayout as Stage
import Types.GameState

-- (Giữ lại cho các module cũ còn import; bố cục mới dùng StageLayout.)
roomMapSize :: Map.MapSize
roomMapSize = Map.SmallMap

-- Chỉ số cơ bản theo nghề.
statsFor :: Role -> Stats
statsFor Warrior = Stats { hp = 150, maxHp = 150, damage = 25, moveSpeed = 4.5, atkSpeed = 0.45, atkRange = 3.0 }
statsFor Archer  = Stats { hp = 100, maxHp = 100, damage = 18, moveSpeed = 5.0, atkSpeed = 0.45, atkRange = 8.0 }

-- Tạo ván mới: màn 0 (phòng đầu, không quái, cửa mở).
newMapState :: [(Int, PlayerProfile)] -> MapState
newMapState profs = enterStage 0 base
  where
    base = MapState
      { mapIndex  = 0
      , isBossMap = False
      , players   = zipWith mkPlayer [0 ..] profs
      , monsters  = []
      , bullets   = []
      , drops     = []
      , portal    = Portal False (0, 0)
      }

    mkPlayer :: Int -> (Int, PlayerProfile) -> Player
    mkPlayer _ (pid, prof) = Player
      { playerId    = pid
      , position    = (0, 0)
      , stats       = statsFor (profileRole prof)
      , status      = Alive
      , pendingExp  = 0
      , pendingGold = 0
      }

-- Vào màn s: đặt lại vị trí người chơi còn sống ở phía dưới phòng, sinh quái của màn,
-- xoá đạn. Vàng / EXP / máu của người chơi được giữ nguyên.
enterStage :: Int -> MapState -> MapState
enterStage s ms = ms
  { mapIndex  = s
  , isBossMap = s == Stage.lastStage
  , players   = zipWith place [0 :: Int ..] (players ms)
  , monsters  = zipWith mkMonster [1 :: Int ..] (Stage.stageSpawns s)
  , bullets   = []
  , portal    = Portal False (Map.toWorldPosition Stage.exitZoneCenter)
  }
  where
    place i p = p { position = Map.toWorldPosition (Stage.entrySpawn i) }

    mkMonster (idx :: Int) sp = Monster
      { monsterId = if s == Stage.lastStage then 900 else 100 * s + idx
      , mPosition = Map.toWorldPosition (Stage.ssPos sp)
      , mStats    = Stats
          { hp = Stage.ssHp sp, maxHp = Stage.ssHp sp
          , damage = Stage.ssDamage sp
          , moveSpeed = 68 / Map.worldScale
          , atkSpeed = Stage.ssAttackRate sp
          , atkRange = 3.0 }
      , kind      = Stage.ssKind sp
      , mCooldown = 0
      }
