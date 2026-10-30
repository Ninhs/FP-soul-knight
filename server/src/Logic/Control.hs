-- Áp input của người chơi (di chuyển + tấn công) vào MapState mỗi tick.
module Logic.Control
  ( PlayerInput(..)
  , idleInput
  , Cooldowns
  , applyInputs
  , clearedRooms
  ) where

import Data.List (foldl')
import qualified Data.Map as M

import qualified MapLayout as Map
import Logic.Combat
  ( AttackResult(..), isPlayerAlive, playerAttack, selectAutoTarget )
import Logic.MapGen (roomMapSize)
import Logic.Movement (moveWithAxisCollision, stepFromInput)
import Types.GameState

-- Input mới nhất server nhận được từ một người chơi.
data PlayerInput = PlayerInput
  { inMove   :: (Float, Float)
  , inAttack :: Bool
  } deriving (Show)

idleInput :: PlayerInput
idleInput = PlayerInput (0, 0) False

-- Thời gian hồi đòn còn lại của từng người chơi (giây), key = playerId.
type Cooldowns = M.Map Int Float

-- Phòng đã dọn sạch quái = không còn quái nào có "phòng nhà" là phòng đó.
clearedRooms :: MapState -> [Map.RoomId]
clearedRooms ms =
  [ r
  | r <- [Map.NormalA, Map.NormalB, Map.BossRoom]
  , not (any ((== Just r) . Map.homeRoomOfMonster . monsterId) (monsters ms))
  ]

applyInputs :: Float -> M.Map Int PlayerInput -> Cooldowns -> MapState -> (MapState, Cooldowns)
applyInputs dt inputs cds0 ms0 = foldl' step (ms0, cds0) (map playerId (players ms0))
  where
    opened = Map.openLinks (clearedRooms ms0)
    blocked pos = Map.isBlocked roomMapSize opened Map.playerHitRadius (Map.toScreenPosition pos)

    step (ms, cds) pid = case filter ((== pid) . playerId) (players ms) of
      (p : _) | isPlayerAlive p ->
        let inp = M.findWithDefault idleInput pid inputs
            speed = moveSpeed (stats p)
            delta = stepFromInput (inMove inp) speed dt
            moved = p { position = moveWithAxisCollision blocked (position p) delta }
            cd = max 0 (M.findWithDefault 0 pid cds - dt)
            (ms', cd') = tryAttack inp moved cd ms
            ms'' = ms' { players = map (\q -> if playerId q == pid then moved else q) (players ms') }
        in (ms'', M.insert pid cd' cds)
      _ -> (ms, cds)

    tryAttack inp p cd ms
      | not (inAttack inp) || cd > 0 = (ms, cd)
      | otherwise = case selectAutoTarget p (monsters ms) of
          Nothing -> (ms, cd)
          Just target ->
            let cooldown = max 0.1 (atkSpeed (stats p))
            in case playerAttack p target of
                 MeleeHit m' -> (ms { monsters = map (replaceMonster m') (monsters ms) }, cooldown)
                 Shot b      -> (ms { bullets = b : bullets ms }, cooldown)

    replaceMonster m' m = if monsterId m == monsterId m' then m' else m
