module Logic.Spawn (spawnMonsters) where

import System.Random (StdGen, randomR)

import Types.GameState
  ( MapTemplate(..), Monster(..), MonsterKind(..)
  , AttackType(..), BodyType(..), Stats(..) )


monsterCount :: Int
monsterCount = 15            -- số quái thường

bossCount :: Int
bossCount = 1                -- số boss

monsterPool :: [MonsterKind] -- các loại quái thường (không được rỗng)
monsterPool =
  [ Regular Melee  TankBody
  , Regular Ranged GlassBody
  ]

-- Sinh toàn bộ quái của map: monsterCount quái thường (random toàn map)
--   + bossCount boss. Trả về danh sách quái và bộ random mới.
--   firstId: id bắt đầu để monsterId không trùng nhau.
spawnMonsters :: MapTemplate -> Int -> StdGen -> ([Monster], StdGen)
spawnMonsters tpl firstId g0 = (regulars ++ bosses, g1)
  where
    (minX, maxX, minY, maxY) = mapBounds tpl

    (regulars, g1) = spawnRegulars firstId monsterCount g0

    -- boss: id nối tiếp sau quái thường, đứng giữa map, lệch nhau 60 theo trục x
    bosses = [ mkMonster (firstId + monsterCount + j) (cx + 60 * fromIntegral j, cy) Boss
             | j <- [0 .. bossCount - 1] ]

    cx = (minX + maxX) / 2
    cy = (minY + maxY) / 2

    spawnRegulars :: Int -> Int -> StdGen -> ([Monster], StdGen)
    spawnRegulars _ 0 g = ([], g)
    spawnRegulars i n g =
      let (x, ga)      = randomR (minX, maxX) g
          (y, gb)      = randomR (minY, maxY) ga
          (k, gc)      = pick monsterPool gb
          (rest, gEnd) = spawnRegulars (i + 1) (n - 1) gc
      in (mkMonster i (x, y) k : rest, gEnd)

-- Chọn ngẫu nhiên 1 phần tử (danh sách phải không rỗng).
pick :: [a] -> StdGen -> (a, StdGen)
pick xs g =
  let (i, g') = randomR (0, length xs - 1) g
  in (xs !! i, g')

mkMonster :: Int -> (Float, Float) -> MonsterKind -> Monster
mkMonster i pos k = Monster
  { monsterId = i
  , mPosition = pos
  , mStats    = statsFor k
  , kind      = k
  , mCooldown = 0
  }

statsFor :: MonsterKind -> Stats
statsFor Boss = Stats
  { hp = 1500, maxHp = 1500, damage = 25
  , moveSpeed = 70, atkSpeed = 1.0, atkRange = 300 }
statsFor (Regular atk body) = Stats
  { hp = h, maxHp = h, damage = d
  , moveSpeed = spd, atkSpeed = 1.0, atkRange = rng }
  where
    (h, d, spd) = case body of
      TankBody  -> (150, 8, 60)
      GlassBody -> (50, 15, 90)
    rng = case atk of
      Melee  -> 30
      Ranged -> 250