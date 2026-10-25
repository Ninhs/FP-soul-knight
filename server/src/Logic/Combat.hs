module Logic.Combat
    ( -- auto-target
      selectAutoTarget
      -- tính sát thương
    , bodyMultiplier
    , damageToMonster
    , applyDamageToMonster
    , applyDamageToPlayer
    , isMonsterAlive
    , isPlayerAlive
    , splitDeadMonsters
      -- va chạm đạn
    , circlesOverlap
    , AttackResult(..)
    , playerAttack
    , fireBullet
    , bulletHitsMonster
    , bulletHitsPlayer
    , stepBullets
    , resolvePlayerBullets
    , resolveMonsterBullets
    ) where

import Data.List (minimumBy, partition)
import Data.Ord (comparing)

import Logic.Movement (Vec2, addVec, distance, normalize, scaleVec, subVec)
import Types.Item
import Types.Monster
import Types.Player

--  LUẬT AUTO-TARGET QUÁI (gần nhất > máu thấp nhất)

-- Chọn quái để đánh: chỉ xét quái còn sống TRONG TẦM ĐÁNH (atkRange)
-- Ưu tiên gần nhất; bằng nhau thì máu thấp nhất; vẫn bằng thì lấy con đầu danh sách
-- Trả Nothing khi không có quái nào trong tầm
selectAutoTarget :: Player -> [Monster] -> Maybe Monster
selectAutoTarget p msList = case filter inRange msList of
    []    -> Nothing
    cands -> Just (minimumBy (comparing key) cands)
  where
    pPos = position p
    inRange m = hp (mStats m) > 0 && distance pPos (mPosition m) <= atkRange (stats p)
    key m = (distance pPos (mPosition m), hp (mStats m))

--  TÍNH SÁT THƯƠNG

-- Hệ số sát thương nhận vào theo thể chất quái
-- TankBody trâu (nhận 70%), GlassBody giòn (nhận 130%), Boss nhận nguyên 100%.
bodyMultiplier :: Monster -> Float
bodyMultiplier m = case kind m of
    Boss                 -> 1.0
    Regular _ TankBody   -> 0.7
    Regular _ GlassBody  -> 1.3

-- Sát thương thực tế quái nhận từ một đòn có sát thương gốc rawDmg.
-- Đòn có sát thương > 0 thì luôn gây ít nhất 1 máu.
damageToMonster :: Int -> Monster -> Int
damageToMonster rawDmg m
    | rawDmg <= 0 = 0
    | otherwise   = max 1 (round (fromIntegral rawDmg * bodyMultiplier m))

-- Trừ máu quái (không để âm).
applyDamageToMonster :: Int -> Monster -> Monster
applyDamageToMonster rawDmg m = m { mStats = st { hp = max 0 (hp st - damageToMonster rawDmg m) } }
  where st = mStats m

-- Trừ máu người chơi; về 0 máu thì chuyển sang Dead.
-- Người đã Dead/Disconnected không nhận thêm sát thương.
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

-- Tách danh sách quái thành (còn sống, đã chết). GameLoop dùng danh sách "đã chết"
-- để gọi dropItemsFromMonster.
splitDeadMonsters :: [Monster] -> ([Monster], [Monster])
splitDeadMonsters = partition isMonsterAlive

-- Hai hình tròn có chạm nhau không.
circlesOverlap :: Vec2 -> Float -> Vec2 -> Float -> Bool
circlesOverlap posA radiusA posB radiusB =
    distance posA posB <= max 0 radiusA + max 0 radiusB


--  ĐẠN & VA CHẠM ĐẠN

-- Hằng số (đơn vị cùng hệ với pickupDistance / atkRange)
bulletSpeed, bulletRadius, playerRadius, monsterRadius, meleeMaxRange :: Float
bulletSpeed   = 12.0
bulletRadius  = 0.2
playerRadius  = 0.5
monsterRadius = 0.6
meleeMaxRange = 3.0   -- atkRange <= ngưỡng này thì đánh cận chiến, lớn hơn thì bắn đạn

-- Hitbox hình tròn của quái (Boss to hơn).
monsterBodyRadius :: Monster -> Float
monsterBodyRadius m = if kind m == Boss then monsterRadius * 2 else monsterRadius

-- Kết quả một lần đánh: cận chiến trúng ngay (trả quái đã trừ máu) hoặc sinh ra viên đạn.
data AttackResult = MeleeHit Monster | Shot Bullet
    deriving (Show)

-- Người chơi tấn công mục tiêu đã chọn bằng selectAutoTarget.
-- Cooldown (atkSpeed) do GameLoop quản lý, hàm này chỉ tính kết quả của một đòn.
playerAttack :: Player -> Monster -> AttackResult
playerAttack p m
    | atkRange (stats p) <= meleeMaxRange = MeleeHit (applyDamageToMonster (damage (stats p)) m)
    | otherwise                           = Shot (fireBullet p m)

-- Tạo đạn bay từ người chơi về phía quái (bOwner = playerId).
fireBullet :: Player -> Monster -> Bullet
fireBullet p m = Bullet
    { bPosition = position p
    , bVelocity = scaleVec bulletSpeed (normalize (subVec (mPosition m) (position p)))
    , bOwner    = playerId p
    , bDamage   = damage (stats p)
    }

bulletHitsMonster :: Bullet -> Monster -> Bool
bulletHitsMonster b m =
    isMonsterAlive m && circlesOverlap (bPosition b) bulletRadius (mPosition m) (monsterBodyRadius m)

bulletHitsPlayer :: Bullet -> Player -> Bool
bulletHitsPlayer b p =
    isPlayerAlive p && circlesOverlap (bPosition b) bulletRadius (position p) playerRadius

-- Cho tất cả đạn bay một bước dt giây; đạn chạm tường (isBlocked) bị xoá.
stepBullets :: (Vec2 -> Bool) -> Float -> [Bullet] -> [Bullet]
stepBullets isBlocked dt = filter (not . isBlocked . bPosition) . map move
  where move b = b { bPosition = addVec (bPosition b) (scaleVec dt (bVelocity b)) }

-- Xử lý đạn của NGƯỜI CHƠI bắn trúng quái: mỗi viên trúng con quái gần nhất
-- chạm phải, trừ máu rồi biến mất. Trả về (đạn còn bay, danh sách quái sau khi trừ máu).
resolvePlayerBullets :: [Bullet] -> [Monster] -> ([Bullet], [Monster])
resolvePlayerBullets bs ms0 = (reverse flying, ms')
  where
    (flying, ms') = foldl step ([], ms0) bs

    step (acc, ms) b = case filter (bulletHitsMonster b) ms of
        [] -> (b : acc, ms)
        hs ->
            let target = minimumBy (comparing (distance (bPosition b) . mPosition)) hs
                hit m | monsterId m == monsterId target = applyDamageToMonster (bDamage b) m
                      | otherwise                       = m
            in (acc, map hit ms)

-- Xử lý đạn của QUÁI bắn trúng người chơi (người gọi chỉ truyền vào đạn của quái).
-- Trả về (đạn còn bay, danh sách người chơi sau khi trừ máu).
resolveMonsterBullets :: [Bullet] -> [Player] -> ([Bullet], [Player])
resolveMonsterBullets bs ps0 = (reverse flying, ps')
  where
    (flying, ps') = foldl step ([], ps0) bs

    step (acc, ps) b = case filter (bulletHitsPlayer b) ps of
        [] -> (b : acc, ps)
        hs ->
            let target = minimumBy (comparing (distance (bPosition b) . position)) hs
                hit p | playerId p == playerId target = applyDamageToPlayer (bDamage b) p
                      | otherwise                     = p
            in (acc, map hit ps)
