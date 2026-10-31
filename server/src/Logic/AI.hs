module Logic.AI
  ( Vec2
  , nearestBy
  , directionToTarget
  , shouldChase
  , AIState(..)
  , RangedMove(..)
  , decideAIState
  , rangedMove
  , decideRangedState
  , moveDirectionFor
  , updateAI
  ) where

import Data.Maybe (fromMaybe)

import qualified MapLayout as Map
import qualified StageLayout as Stage
import Logic.Movement (Vec2, addVec, distance, moveWithAxisCollision, normalize, scaleVec, subVec)
import Logic.Combat (MonsterAttackResult(..), monsterAttack, isPlayerAlive, isMonsterAlive)
import Types.GameState

-- Chọn mục tiêu gần nhất, giữ nguyên cả giá trị (id, player...).
-- Chỉ truyền vào người chơi đủ điều kiện (còn sống, còn kết nối).
nearestBy :: (a -> Vec2) -> Vec2 -> [a] -> Maybe a
nearestBy _ _ [] = Nothing
nearestBy getPos origin (first:rest) =
  Just (foldl nearer first rest)
  where
    nearer best candidate
      | distance origin (getPos candidate) < distance origin (getPos best) = candidate
      | otherwise = best

directionToTarget :: Vec2 -> Vec2 -> Vec2
directionToTarget fromPos targetPos = normalize (subVec targetPos fromPos)

shouldChase :: Float -> Vec2 -> Vec2 -> Bool
shouldChase aggroRange fromPos targetPos =
  distance fromPos targetPos <= max 0 aggroRange

data AIState
  = Idle
  | Chasing
  | Attacking
  | KeepingDistance
  deriving (Eq, Show, Read)

-- Quái cận chiến.
decideAIState :: Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideAIState _ _ Nothing _ = Idle
decideAIState aggroRange attackRange (Just targetPos) monsterPos
  | not (shouldChase aggroRange monsterPos targetPos) = Idle
  | distance monsterPos targetPos <= max 0 attackRange = Attacking
  | otherwise = Chasing

data RangedMove
  = Retreat
  | Approach
  | Hold
  deriving (Eq, Show)

rangedMove :: Float -> Float -> Vec2 -> Vec2 -> RangedMove
rangedMove minimumRange preferredRange monsterPos targetPos
  | d < max 0 minimumRange = Retreat
  | d > max minimumRange preferredRange = Approach
  | otherwise = Hold
  where d = distance monsterPos targetPos

decideRangedState :: Float -> Float -> Float -> Maybe Vec2 -> Vec2 -> AIState
decideRangedState _ _ _ Nothing _ = Idle
decideRangedState aggroRange minimumRange preferredRange (Just targetPos) monsterPos
  | not (shouldChase aggroRange monsterPos targetPos) = Idle
  | otherwise =
      case rangedMove minimumRange preferredRange monsterPos targetPos of
        Retreat  -> KeepingDistance
        Approach -> Chasing
        Hold     -> Attacking

moveDirectionFor :: AIState -> Vec2 -> Vec2 -> Vec2
moveDirectionFor st monsterPos targetPos =
  case st of
    Chasing         -> toward
    KeepingDistance -> scaleVec (-1) toward
    _               -> (0, 0)
  where toward = directionToTarget monsterPos targetPos

--  CẤU HÌNH AI QUÁI (chỉnh số ở đây để tinh chỉnh độ khó) --

monsterAggroRange :: Float
monsterAggroRange = 14.0

meleeAttackRange :: Float
meleeAttackRange = 1.0

rangedMinRangeC, rangedPreferredRangeC :: Float
rangedMinRangeC       = 2.5
rangedPreferredRangeC = 5.5

-- Thời gian hồi chiêu (giây) giữa 2 đòn đánh, theo loại quái.
attackCooldownFor :: MonsterKind -> Float
attackCooldownFor Boss               = 0.6
attackCooldownFor (Regular Melee _)  = 0.9
attackCooldownFor (Regular Ranged _) = 1.2

--  CẬP NHẬT AI (mỗi tick) --

-- Với mỗi quái còn sống: chọn người chơi gần nhất còn sống làm mục tiêu,
-- xác định trạng thái AI, di chuyển theo trạng thái đó, và ra đòn
-- (cận chiến trừ máu ngay / tầm xa sinh đạn) khi đang Attacking và đã
-- hết cooldown riêng của quái đó (mCooldown).
updateAI :: Float -> MapState -> MapState
updateAI dt ms = ms
  { monsters = reverse monstersOut
  , players  = playersOut
  , bullets  = bullets ms ++ reverse bulletsOut
  }
  where
    (monstersOut, playersOut, bulletsOut) =
      foldl stepMonster ([], players ms, []) (monsters ms)

    -- Quái không ra khỏi vùng sàn của phòng
    monsterBlocked pos =
      Stage.blockedByWalls Map.monsterHitRadius (Map.toScreenPosition pos)

    stepMonster (accM, currentPlayers, accB) m
      | not (isMonsterAlive m) = (m : accM, currentPlayers, accB)
      | otherwise =
          let target        = nearestBy position (mPosition m) (filter isPlayerAlive currentPlayers)
              targetPos      = fmap position target
              aiState        = case kind m of
                                  Regular Ranged _ ->
                                    decideRangedState monsterAggroRange rangedMinRangeC rangedPreferredRangeC targetPos (mPosition m)
                                  _ ->
                                    decideAIState monsterAggroRange meleeAttackRange targetPos (mPosition m)
              dir            = moveDirectionFor aiState (mPosition m) (fromMaybe (mPosition m) targetPos)
              speed          = moveSpeed (mStats m)
              newPos         = moveWithAxisCollision monsterBlocked (mPosition m) (scaleVec (speed * dt) dir)
              cooldownAfter  = max 0 (mCooldown m - dt)
              canAttack      = aiState == Attacking && cooldownAfter <= 0
          in case (canAttack, target) of
               (True, Just tgt) ->
                 case monsterAttack (m { mPosition = newPos }) tgt of
                   MonsterMeleeHit hitPlayer ->
                     ( m { mPosition = newPos, mCooldown = attackCooldownFor (kind m) } : accM
                     , replacePlayer hitPlayer currentPlayers
                     , accB
                     )
                   MonsterShot bullet ->
                     ( m { mPosition = newPos, mCooldown = attackCooldownFor (kind m) } : accM
                     , currentPlayers
                     , bullet : accB
                     )
               _ ->
                 ( m { mPosition = newPos, mCooldown = cooldownAfter } : accM
                 , currentPlayers
                 , accB
                 )

    replacePlayer updated = map (\p -> if playerId p == playerId updated then updated else p)