module Logic.Combat
    ( -- auto-target
      selectAutoTarget
      -- tính sát thương
    , damageToMonster
    , applyDamageToMonster
    , applyDamageToPlayer
    , isMonsterAlive
    , isPlayerAlive
    , splitDeadMonsters
      -- va chạm
    , circlesOverlap
      -- người chơi tấn công
    , AttackResult(..)
    , playerAttack
    , fireBullet
      -- quái tấn công
    , MonsterAttackResult(..)
    , monsterAttack
    , fireMonsterBullet
      -- đạn
    , bulletHitsMonster
    , bulletHitsPlayer
    , stepBullets
    , resolvePlayerBullets
    , resolveMonsterBullets
    ) where

import Data.List (minimumBy, partition)
import Data.Ord (comparing)

import Logic.Movement (Vec2, addVec, distance, normalize, scaleVec, subVec)
import Types.GameState

bulletSpeed, monsterBulletSpeed :: Float
bulletSpeed        = 12.0
monsterBulletSpeed = 9.0

bulletRadius, playerRadius, monsterRadius, meleeMaxRange :: Float
bulletRadius  = 0.2
playerRadius  = 0.5
monsterRadius = 0.6
meleeMaxRange = 3.0

monsterBodyRadius :: Monster -> Float
monsterBodyRadius m = if kind m == Boss then monsterRadius * 2 else monsterRadius

selectAutoTarget :: Player -> [Monster] -> Maybe Monster
selectAutoTarget p msList = case filter inRange msList of
    []    -> Nothing
    cands -> Just (minimumBy (comparing key) cands)
  where
    pPos = position p
    inRange m = isMonsterAlive m
             && distance pPos (mPosition m) - monsterBodyRadius m <= playerAttackRange
    key m = (distance pPos (mPosition m), hp (mStats m))

playerAttackRange :: Float
playerAttackRange = 3.0

damageToMonster :: Int -> Monster -> Int
damageToMonster rawDmg _ = max 0 rawDmg

applyDamageToMonster :: Int -> Monster -> Monster
applyDamageToMonster rawDmg m = m { mStats = st { hp = max 0 (hp st - damageToMonster rawDmg m) } }
  where st = mStats m

applyDamageToPlayer :: Int -> Player -> Player
applyDamageToPlayer rawDmg p
    | status p /= Alive = p
    | otherwise         = p { stats = st { hp = newHp }
                            , status = if newHp <= 0 then Dead else Alive }
  where
    st    = stats p
    newHp = max 0 (hp st - max 0 rawDmg)

isMonsterAlive :: Monster -> Bool
isMonsterAlive m = hp (mStats m) > 0

isPlayerAlive :: Player -> Bool
isPlayerAlive p = status p == Alive && hp (stats p) > 0

splitDeadMonsters :: [Monster] -> ([Monster], [Monster])
splitDeadMonsters = partition isMonsterAlive

circlesOverlap :: Vec2 -> Float -> Vec2 -> Float -> Bool
circlesOverlap posA radiusA posB radiusB =
    distance posA posB <= max 0 radiusA + max 0 radiusB

aimDir :: Vec2 -> Vec2 -> Vec2
aimDir from to
    | d == (0, 0) = (1, 0)
    | otherwise   = d
  where d = normalize (subVec to from)

data AttackResult = MeleeHit Monster | Shot Bullet
    deriving (Show)

playerAttack :: Player -> Monster -> AttackResult
playerAttack p m
    | playerAttackRange <= meleeMaxRange = MeleeHit (applyDamageToMonster (damage (stats p)) m)
    | otherwise                           = Shot (fireBullet p m)

fireBullet :: Player -> Monster -> Bullet
fireBullet p m = Bullet
    { bPosition = position p
    , bVelocity = scaleVec bulletSpeed (aimDir (position p) (mPosition m))
    , bOwner    = encodePlayerOwner (playerId p)
    , bDamage   = damage (stats p)
    }

data MonsterAttackResult = MonsterMeleeHit Player | MonsterShot Bullet
    deriving (Show)

monsterAttack :: Monster -> Player -> MonsterAttackResult
monsterAttack m p = case kind m of
    Regular Ranged _ -> MonsterShot (fireMonsterBullet m p)
    _                -> MonsterMeleeHit (applyDamageToPlayer (damage (mStats m)) p)

fireMonsterBullet :: Monster -> Player -> Bullet
fireMonsterBullet m p = Bullet
    { bPosition = mPosition m
    , bVelocity = scaleVec monsterBulletSpeed (aimDir (mPosition m) (position p))
    , bOwner    = encodeMonsterOwner (monsterId m)
    , bDamage   = damage (mStats m)
    }

encodePlayerOwner :: Int -> Int
encodePlayerOwner = (+ 1) . abs

encodeMonsterOwner :: Int -> Int
encodeMonsterOwner = negate . (+ 1) . abs

bulletHitsMonster :: Bullet -> Monster -> Bool
bulletHitsMonster b m =
    isMonsterAlive m && circlesOverlap (bPosition b) bulletRadius (mPosition m) (monsterBodyRadius m)

bulletHitsPlayer :: Bullet -> Player -> Bool
bulletHitsPlayer b p =
    isPlayerAlive p && circlesOverlap (bPosition b) bulletRadius (position p) playerRadius

stepBullets :: (Vec2 -> Bool) -> Float -> [Bullet] -> [Bullet]
stepBullets isBlocked dt = filter alive . map move
  where
    move b = b { bPosition = addVec (bPosition b) (scaleVec dt (bVelocity b)) }
    alive b = not (isBlocked (bPosition b))

resolvePlayerBullets :: [Bullet] -> [Monster] -> ([Bullet], [Monster])
resolvePlayerBullets bs ms0 = (reverse flying, ms')
  where
    (flying, ms') = foldl step ([], ms0) bs

    step (acc, ms) b
      | bOwner b <= 0 = (b : acc, ms)
      | otherwise = case filter (bulletHitsMonster b) ms of
            [] -> (b : acc, ms)
            hs ->
                let target = minimumBy (comparing (distance (bPosition b) . mPosition)) hs
                    hit m | monsterId m == monsterId target = applyDamageToMonster (bDamage b) m
                          | otherwise                       = m
                in (acc, map hit ms)

resolveMonsterBullets :: [Bullet] -> [Player] -> ([Bullet], [Player])
resolveMonsterBullets bs ps0 = (reverse flying, ps')
  where
    (flying, ps') = foldl step ([], ps0) bs

    step (acc, ps) b
      | bOwner b > 0 = (b : acc, ps)
      | otherwise = case filter (bulletHitsPlayer b) ps of
            [] -> (b : acc, ps)
            hs ->
                let target = minimumBy (comparing (distance (bPosition b) . position)) hs
                    hit p | playerId p == playerId target = applyDamageToPlayer (bDamage b) p
                          | otherwise                     = p
                in (acc, map hit ps)  