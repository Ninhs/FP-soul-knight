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

-- Combat chỉ trả lời: "có một đòn / một viên đạn thì kết quả là gì"
-- Ai quyết định đánh (AI.hs), khi nào được đánh (cooldown, GameLoop.hs)

--  HẰNG SỐ (đơn vị cùng hệ với pickupDistance / atkRange)

bulletSpeed, monsterBulletSpeed, bulletLifetime :: Float
bulletSpeed        = 12.0  -- tốc độ đạn người chơi
monsterBulletSpeed = 9.0   -- tốc độ đạn quái 
bulletLifetime     = 2.0   -- thời gian đạn tự biến mất sau khi bắn (giây)

bulletRadius, playerRadius, monsterRadius, meleeMaxRange :: Float
bulletRadius  = 0.2   -- hitbox đạn
playerRadius  = 0.5   -- hitbox người chơi
monsterRadius = 0.6   -- hitbox quái
meleeMaxRange = 3.0   -- atkRange <= ngưỡng này thì đánh cận chiến, lớn hơn thì bắn đạn(quyết định người chơi đánh kiểu gì)

-- Hitbox hình tròn của quái (Boss to hơn).
monsterBodyRadius :: Monster -> Float
monsterBodyRadius m = if kind m == Boss then monsterRadius * 2 else monsterRadius

--  LUẬT AUTO-TARGET QUÁI (gần nhất > máu thấp nhất)

-- Chọn quái để đánh chỉ xét quái còn sống TRONG TẦM ĐÁNH (atkRange).
-- Khoảng cách tính tới MÉP hitbox quái, nên đứng sát boss to vẫn đánh được.
-- Ưu tiên gần nhất; bằng nhau thì máu thấp nhất; vẫn bằng thì lấy con đầu danh sách.
-- Trả Nothing khi không có quái nào trong tầm.
selectAutoTarget :: Player -> [Monster] -> Maybe Monster
selectAutoTarget p msList = case filter inRange msList of
    []    -> Nothing
    cands -> Just (minimumBy (comparing key) cands)
  where
    pPos = position p
    inRange m = isMonsterAlive m
             && distance pPos (mPosition m) - monsterBodyRadius m <= atkRange (stats p)
    key m = (distance pPos (mPosition m), hp (mStats m))

--  TÍNH SÁT THƯƠNG

-- Hệ số sát thương nhận vào theo thể chất quái

-- Đòn có sát thương > 0 thì luôn gây ít nhất 1 máu.
damageToMonster :: Int -> Monster -> Int
damageToMonster rawDmg _ = max 0 rawDmg

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

-- Tách danh sách quái thành (còn sống, đã chết). GameLoop dùng danh sách "đã chết" để rơi đồ
splitDeadMonsters :: [Monster] -> ([Monster], [Monster])
splitDeadMonsters = partition isMonsterAlive

-- Hai hình tròn có chạm nhau không (hitbox chạm nhau)
circlesOverlap :: Vec2 -> Float -> Vec2 -> Float -> Bool
circlesOverlap posA radiusA posB radiusB =
    distance posA posB <= max 0 radiusA + max 0 radiusB

-- Hướng từ điểm này tới điểm kia. Hai điểm trùng nhau thì bắn sang phải (1,0) thay vì (0,0), tránh đạn đứng yên.
aimDir :: Vec2 -> Vec2 -> Vec2
aimDir from to
    | d == (0, 0) = (1, 0)
    | otherwise   = d
  where d = normalize (subVec to from)

--  NGƯỜI CHƠI TẤN CÔNG

-- Kết quả một lần đánh: cận chiến trúng ngay (trả quái đã trừ máu) hoặc sinh ra viên đạn.
data AttackResult = MeleeHit Monster | Shot Bullet
    deriving (Show)

-- Người chơi tấn công mục tiêu đã chọn bằng selectAutoTarget.
-- Cooldown (atkSpeed) do GameLoop quản lý, hàm này chỉ tính kết quả của một đòn.
playerAttack :: Player -> Monster -> AttackResult
playerAttack p m
    | atkRange (stats p) <= meleeMaxRange = MeleeHit (applyDamageToMonster (damage (stats p)) m)
    | otherwise                           = Shot (fireBullet p m)

-- Tạo đạn bay từ người chơi về phía quái (bOwner = FromPlayer playerId).
fireBullet :: Player -> Monster -> Bullet
fireBullet p m = Bullet
    { bPosition = position p
    , bVelocity = scaleVec bulletSpeed (aimDir (position p) (mPosition m))
    , bOwner    = FromPlayer (playerId p)
    , bDamage   = damage (stats p)
    , bLife     = bulletLifetime
    }

--  QUÁI TẤN CÔNG

-- Kết quả một lần quái đánh: cận chiến trúng ngay (trả người đã trừ máu) hoặc sinh đạn.
data MonsterAttackResult = MonsterMeleeHit Player | MonsterShot Bullet
    deriving (Show)

-- Quái tấn công người chơi mục tiêu (AI.hs đã quyết định là Attacking, GameLoop đã kiểm tra cooldown). Kiểu đánh theo loại quái:
--   Regular Ranged -> bắn đạn
--   Regular Melee, (Boss) -> cận chiến (Boss luôn đánh gần)
monsterAttack :: Monster -> Player -> MonsterAttackResult
monsterAttack m p = case kind m of
    Regular Ranged _ -> MonsterShot (fireMonsterBullet m p)
    _                -> MonsterMeleeHit (applyDamageToPlayer (damage (mStats m)) p)

-- Tạo đạn bay từ quái về phía người chơi (bOwner = FromMonster monsterId).
fireMonsterBullet :: Monster -> Player -> Bullet
fireMonsterBullet m p = Bullet
    { bPosition = mPosition m
    , bVelocity = scaleVec monsterBulletSpeed (aimDir (mPosition m) (position p))
    , bOwner    = FromMonster (monsterId m)
    , bDamage   = damage (mStats m)
    , bLife     = bulletLifetime
    }

--  ĐẠN & VA CHẠM ĐẠN

bulletHitsMonster :: Bullet -> Monster -> Bool
bulletHitsMonster b m =
    isMonsterAlive m && circlesOverlap (bPosition b) bulletRadius (mPosition m) (monsterBodyRadius m)

bulletHitsPlayer :: Bullet -> Player -> Bool
bulletHitsPlayer b p =
    isPlayerAlive p && circlesOverlap (bPosition b) bulletRadius (position p) playerRadius

-- Cho tất cả đạn bay một bước dt giây.
-- Đạn bị xoá khi chạm tường (isBlocked) hoặc hết thời gian sống (bLife <= 0).
stepBullets :: (Vec2 -> Bool) -> Float -> [Bullet] -> [Bullet]
stepBullets isBlocked dt = filter alive . map move
  where
    move b = b { bPosition = addVec (bPosition b) (scaleVec dt (bVelocity b))
               , bLife     = bLife b - dt }
    alive b = bLife b > 0 && not (isBlocked (bPosition b))

-- Xử lý đạn của NGƯỜI CHƠI (bOwner = FromPlayer _) bắn trúng quái: mỗi viên trúng
-- con quái gần nhất chạm phải, trừ máu rồi biến mất. Đạn của quái được giữ nguyên, nên có thể truyền cả danh sách đạn của phòng vào.
-- Trả về (đạn còn bay, danh sách quái sau khi trừ máu).
resolvePlayerBullets :: [Bullet] -> [Monster] -> ([Bullet], [Monster])
resolvePlayerBullets bs ms0 = (reverse flying, ms')
  where
    (flying, ms') = foldl step ([], ms0) bs

    step (acc, ms) b = case bOwner b of
        FromMonster _ -> (b : acc, ms)
        FromPlayer _  -> case filter (bulletHitsMonster b) ms of
            [] -> (b : acc, ms)
            hs ->
                let target = minimumBy (comparing (distance (bPosition b) . mPosition)) hs
                    hit m | monsterId m == monsterId target = applyDamageToMonster (bDamage b) m
                          | otherwise                       = m
                in (acc, map hit ms)

-- Xử lý đạn của QUÁI (bOwner = FromMonster _) bắn trúng người chơi. Đạn của người chơi được giữ nguyên (không bắn trúng đồng đội).
-- Trả về (đạn còn bay, danh sách người chơi sau khi trừ máu).
resolveMonsterBullets :: [Bullet] -> [Player] -> ([Bullet], [Player])
resolveMonsterBullets bs ps0 = (reverse flying, ps')
  where
    (flying, ps') = foldl step ([], ps0) bs

    step (acc, ps) b = case bOwner b of
        FromPlayer _  -> (b : acc, ps)
        FromMonster _ -> case filter (bulletHitsPlayer b) ps of
            [] -> (b : acc, ps)
            hs ->
                let target = minimumBy (comparing (distance (bPosition b) . position)) hs
                    hit p | playerId p == playerId target = applyDamageToPlayer (bDamage b) p
                          | otherwise                     = p
                in (acc, map hit ps)
