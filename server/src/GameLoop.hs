module GameLoop
  ( PlayerInput(..)
  , emptyInput
  , LoopState(..)
  , newLoopState
  , newMatch
  , stepMatch
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')

import Logic.AI
import Logic.Combat
import Logic.Movement
import Logic.Portal (updatePortal)
import Types.GameState

-- Input mới nhất của một người chơi, client gửi liên tục
data PlayerInput = PlayerInput
  { inMove          :: (Float, Float)
  , inAim           :: (Float, Float)
  , inAttackPending :: Bool   -- có yêu cầu đánh chưa được xử lý
  } deriving (Show)

emptyInput :: PlayerInput
emptyInput = PlayerInput (0, 0) (0, 0) False

-- Trạng thái chạy game của một phòng (không gửi nguyên cho client)
data LoopState = LoopState
  { lsMap             :: MapState
  , lsInputs          :: Map Int PlayerInput
  , lsPlayerCooldown  :: Map Int Float
  , lsMonsterCooldown :: Map Int Float
  }

newLoopState :: LoopState
newLoopState = LoopState
  { lsMap = MapState
      { mapIndex = 0
      , isBossMap = False
      , players = []
      , monsters = []
      , bullets = []
      , drops = []
      , portal = Portal False (0, 0)
      , deadMonsterIds = []
      }
  , lsInputs = Map.empty
  , lsPlayerCooldown = Map.empty
  , lsMonsterCooldown = Map.empty
  }

-- Tạo trận mới từ danh sách người chơi trong phòng (tối đa 4)
-- TODO: thay bằng map template thật khi có dữ liệu map
newMatch :: [(Int, PlayerProfile)] -> LoopState
newMatch profs = base
  { lsMap = (lsMap base)
      { isBossMap = True
      , players = zipWith mkPlayer playerSpawns profs
      , monsters = demoMonsters
      , portal = Portal False (-10, 0)
      }
  , lsInputs = Map.fromList [ (pid, emptyInput) | (pid, _) <- profs ]
  }
  where
    base = newLoopState

playerSpawns :: [(Float, Float)]
playerSpawns = [(-6, 0), (-6, 2), (-6, -2), (-6, 4)]

mkPlayer :: (Float, Float) -> (Int, PlayerProfile) -> Player
mkPlayer spawn (pid, prof) = Player
  { playerId = pid
  , position = spawn
  , stats = Stats 100 100 10 4 0.5
  , status = Alive
  , pendingExp = 0
  , pendingGold = 0
  , facingRight = True
  , attackSeq = 0
  , playerRole = profileRole prof
  }

demoMonsters :: [Monster]
demoMonsters =
  [ mkMonster 101 (4, 1)   (Regular Melee GlassBody)  (Stats 50 50 8 2.5 1.2)
  , mkMonster 102 (5, -2)  (Regular Melee TankBody)   (Stats 80 80 6 2.0 1.2)
  , mkMonster 103 (8, 3)   (Regular Ranged GlassBody) (Stats 40 40 7 2.2 1.5)
  , mkMonster 104 (10, -4) (Regular Ranged TankBody)  (Stats 60 60 5 2.0 1.6)
  , mkMonster 200 (12, 0)  Boss                       (Stats 300 300 12 1.5 1.0)
  ]
  where
    mkMonster i p k st = Monster
      { monsterId = i
      , mPosition = p
      , mStats = st
      , kind = k
      , mFacingRight = False
      , mAttackSeq = 0
      }

noWall :: Vec2 -> Bool
noWall _ = False

faceTowards :: Float -> Float -> Bool -> Bool
faceTowards from to old
  | to > from = True
  | to < from = False
  | otherwise = old

replacePlayer :: Player -> [Player] -> [Player]
replacePlayer new = map (\p -> if playerId p == playerId new then new else p)

replaceMonster :: Monster -> [Monster] -> [Monster]
replaceMonster new = map (\m -> if monsterId m == monsterId new then new else m)

findPlayer :: Int -> [Player] -> Maybe Player
findPlayer pid ps = case filter ((== pid) . playerId) ps of
  (p : _) -> Just p
  []      -> Nothing

-- Một tick: di chuyển người chơi -> người chơi đánh -> quái ra quyết định và đánh
-- -> đạn bay và va chạm -> xử lý quái chết -> cập nhật cổng.
stepMatch :: Float -> LoopState -> LoopState
stepMatch dt ls =
  let ms       = lsMap ls
      inputs   = lsInputs ls
      pCD0     = Map.map (\c -> max 0 (c - dt)) (lsPlayerCooldown ls)
      mCD0     = Map.map (\c -> max 0 (c - dt)) (lsMonsterCooldown ls)

      players1 = map (movePlayer dt inputs) (players ms)

      (monsters2, bullets2, pCD1, players2) =
        foldl' (playerAct inputs) (monsters ms, bullets ms, pCD0, []) players1

      (players3, bullets3, mCD1, monsters3) =
        foldl' (monsterAct dt) (players2, bullets2, mCD0, []) monsters2

      bullets4 = stepBullets noWall dt bullets3
      (bullets5, monsters4) = resolvePlayerBullets bullets4 monsters3
      (bullets6, players4)  = resolveMonsterBullets bullets5 players3

      (living, dead) = splitDeadMonsters monsters4
      livingIds      = map monsterId living
      mCD2           = Map.filterWithKey (\k _ -> k `elem` livingIds) mCD1

      ms' = ms
        { players = players4
        , monsters = living
        , bullets = bullets6
        , deadMonsterIds = map monsterId dead
        }
  in ls
      { lsMap = updatePortal ms'
      , lsInputs = Map.map clearAttack inputs
      , lsPlayerCooldown = pCD1
      , lsMonsterCooldown = mCD2
      }
  where
    clearAttack i = i { inAttackPending = False }

movePlayer :: Float -> Map Int PlayerInput -> Player -> Player
movePlayer dt inputs p
  | status p /= Alive = p
  | otherwise = p { position = newPos, facingRight = faceAfter }
  where
    input = Map.findWithDefault emptyInput (playerId p) inputs
    (dx, _) = inMove input
    delta = stepFromInput (inMove input) (moveSpeed (stats p)) dt
    newPos = moveWithAxisCollision noWall (position p) delta
    faceAfter
      | dx > 0 = True
      | dx < 0 = False
      | otherwise = facingRight p

-- Người chơi đánh. Trả về: (quái, đạn, cooldown, danh sách người chơi đã xử lý)
playerAct
  :: Map Int PlayerInput
  -> ([Monster], [Bullet], Map Int Float, [Player])
  -> Player
  -> ([Monster], [Bullet], Map Int Float, [Player])
playerAct inputs (mons, bs, cds, done) p
  | status p /= Alive || not wantsAttack || cooldown > 0 = (mons, bs, cds, done ++ [p])
  | otherwise = case selectAutoTarget p mons of
      Nothing -> (mons, bs, cds, done ++ [p])
      Just target ->
        let p1 = p { attackSeq = attackSeq p + 1
                   , facingRight = faceTowards (fst (position p)) (fst (mPosition target)) (facingRight p)
                   }
            cds' = Map.insert pid (max 0.1 (atkSpeed (stats p))) cds
        in case playerAttack p target of
             MeleeHit m' -> (replaceMonster m' mons, bs, cds', done ++ [p1])
             Shot b      -> (mons, bs ++ [b], cds', done ++ [p1])
  where
    pid = playerId p
    wantsAttack = maybe False inAttackPending (Map.lookup pid inputs)
    cooldown = Map.findWithDefault 0 pid cds

-- Quái ra quyết định, di chuyển hoặc đánh.
-- Trả về: (người chơi, đạn, cooldown, danh sách quái đã xử lý)
monsterAct
  :: Float
  -> ([Player], [Bullet], Map Int Float, [Monster])
  -> Monster
  -> ([Player], [Bullet], Map Int Float, [Monster])
monsterAct dt (ps, bs, cds, done) m
  | not (isMonsterAlive m) = (ps, bs, cds, m : done)  -- giữ lại để splitDeadMonsters ghi nhận
  | otherwise = case nearestBy position (mPosition m) (filter isPlayerAlive ps) of
      Nothing -> (ps, bs, cds, m : done)
      Just target0 ->
        let tPos = position target0
            st   = aiStateFor m tPos
            cd   = Map.findWithDefault 0 mid cds
            m1   = m { mFacingRight = faceTowards (fst (mPosition m)) (fst tPos) (mFacingRight m) }
        in if st == Attacking && cd <= 0
             then attackFrom target0 m1
             else (ps, bs, cds, moveMonster dt st m1 tPos : done)
  where
    mid = monsterId m
    attackFrom target0 m1 =
      let m2 = m1 { mAttackSeq = mAttackSeq m1 + 1 }
          cds' = Map.insert mid (max 0.25 (atkSpeed (mStats m1))) cds
          -- lấy người chơi bản mới nhất, vì có thể đã bị quái khác đánh trong tick này
          target = maybe target0 id (findPlayer (playerId target0) ps)
      in case monsterAttack m2 target of
           MonsterMeleeHit p' -> (replacePlayer p' ps, bs, cds', m2 : done)
           MonsterShot b      -> (ps, bs ++ [b], cds', m2 : done)

aiStateFor :: Monster -> (Float, Float) -> AIState
aiStateFor m tPos = case kind m of
  Regular Ranged _ -> decideRangedState aggroRange 3 4.5 (Just tPos) (mPosition m)
  _                -> decideAIState aggroRange 1.1 (Just tPos) (mPosition m)
  where
    aggroRange = 12

moveMonster :: Float -> AIState -> Monster -> (Float, Float) -> Monster
moveMonster dt st m tPos =
  let dir = moveDirectionFor st (mPosition m) tPos
      newPos = moveWithAxisCollision noWall (mPosition m) (scaleVec (moveSpeed (mStats m) * dt) dir)
  in m { mPosition = newPos }
