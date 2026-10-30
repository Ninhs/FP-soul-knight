module NetWorld
  ( NetWorld(..)
  , initNetWorld
  , updateNetWorld
  , stepNetWorld
  , renderNetWorld
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Graphics.Gloss

import Animation
import Assets (GameAssets(..))
import ClientUI (toScreenPosition)
import Logic.Movement (distance)
import qualified Types.GameState as GS

-- Nhóm sprite dùng cho một thực thể
data SpriteSet = SoldierSet | ArcherSet | MonsterSet | OrcSet | DemonSet
  deriving (Eq, Show)

-- Trạng thái hiển thị của một người chơi hoặc quái ở client
data Entity = Entity
  { eSet       :: SpriteSet
  , ePos       :: (Float, Float)
  , eFacing    :: Bool
  , eAttackSeq :: Int
  , eAlive     :: Bool
  , eAnim      :: AnimState
  }

data NetWorld = NetWorld
  { nwState    :: GS.GameState
  , nwPlayers  :: Map Int Entity
  , nwMonsters :: Map Int Entity   -- gồm cả xác quái đã chết (giữ đến hết trận)
  , nwAssets   :: GameAssets
  }

initNetWorld :: GameAssets -> GS.GameState -> NetWorld
initNetWorld assets gs = updateNetWorld gs (NetWorld gs Map.empty Map.empty assets)

-- Gọi khi có StateUpdate từ server
updateNetWorld :: GS.GameState -> NetWorld -> NetWorld
updateNetWorld gs world = case GS.phase gs of
  GS.Playing ms -> world
    { nwState = gs
    , nwPlayers = syncPlayers assets (GS.players ms) (nwPlayers world)
    , nwMonsters = syncMonsters assets (GS.monsters ms) (nwMonsters world)
    }
  GS.Lobby -> world { nwState = gs, nwPlayers = Map.empty, nwMonsters = Map.empty }
  GS.RoomResult _ -> world { nwState = gs }
  where
    assets = nwAssets world

-- Gọi mỗi frame với dt để tiến animation
stepNetWorld :: Float -> NetWorld -> NetWorld
stepNetWorld dt world = world
  { nwPlayers = Map.map tick (nwPlayers world)
  , nwMonsters = Map.map tick (nwMonsters world)
  }
  where
    tick e = e { eAnim = stepAnim dt (eAnim e) }

syncPlayers :: GameAssets -> [GS.Player] -> Map Int Entity -> Map Int Entity
syncPlayers assets ps old = Map.fromList
  [ ( GS.playerId p
    , updateEntity assets (roleSet (GS.playerRole p)) (GS.position p)
        (GS.status p == GS.Alive) (GS.facingRight p) (GS.attackSeq p)
        (Map.lookup (GS.playerId p) old)
    )
  | p <- ps
  ]

syncMonsters :: GameAssets -> [GS.Monster] -> Map Int Entity -> Map Int Entity
syncMonsters assets ms old =
  let alive = Map.fromList
        [ ( GS.monsterId m
          , updateEntity assets (monsterSet (GS.kind m)) (GS.mPosition m) True
              (GS.mFacingRight m) (GS.mAttackSeq m) (Map.lookup (GS.monsterId m) old)
          )
        | m <- ms
        ]
      aliveIds = Map.keys alive
      -- quái có trước đó nhưng không còn trong danh sách sống: vừa chết, giữ xác lại
      corpses = Map.fromList
        [ (mid, killEntity e)
        | (mid, e) <- Map.toList old
        , mid `notElem` aliveIds
        ]
  in Map.union alive corpses

updateEntity
  :: GameAssets -> SpriteSet -> (Float, Float) -> Bool -> Bool -> Int
  -> Maybe Entity -> Entity
updateEntity _ set pos alive facing seqNo Nothing =
  Entity set pos facing seqNo alive (startAnim (if alive then Idle else Die))
updateEntity assets set pos alive facing seqNo (Just old)
  | not alive = (killEntity old) { ePos = pos, eFacing = facing, eAttackSeq = seqNo }
  | seqNo /= eAttackSeq old = keep (startAnim Attack)
  | animKind cur == Attack && not (attackDone assets set cur) = keep cur
  | moved = keep (setAnimKind Walk cur)
  | otherwise = keep (setAnimKind Idle cur)
  where
    cur = eAnim old
    moved = distance pos (ePos old) > 0.0001
    keep anim = Entity set pos facing seqNo True anim

killEntity :: Entity -> Entity
killEntity e
  | eAlive e = e { eAlive = False, eAnim = startAnim Die }
  | otherwise = e

attackDone :: GameAssets -> SpriteSet -> AnimState -> Bool
attackDone assets set st = animFinished (framesOf assets set Attack) st

roleSet :: GS.Role -> SpriteSet
roleSet GS.Warrior = SoldierSet
roleSet GS.Tank    = SoldierSet
roleSet GS.Archer  = ArcherSet
roleSet GS.Mage    = ArcherSet

monsterSet :: GS.MonsterKind -> SpriteSet
monsterSet (GS.Regular GS.Melee _)  = MonsterSet
monsterSet (GS.Regular GS.Ranged _) = OrcSet
monsterSet GS.Boss                  = DemonSet

framesOf :: GameAssets -> SpriteSet -> AnimKind -> [Picture]
framesOf a set anim = case set of
  SoldierSet -> pick (soldierIdle a) (soldierWalk a) (soldierAttack a) (soldierDie a)
  ArcherSet  -> pick (archerIdle a) (archerWalk a) (archerAttack a) (archerDie a)
  MonsterSet -> pick (monsterIdle a) (monsterWalk a) (monsterAttack a) (monsterDie a)
  OrcSet     -> pick (orcIdle a) (orcWalk a) (orcAttack a) (orcDie a)
  DemonSet   -> pick (demonIdle a) (demonWalk a) (demonAttack a) (demonDie a)
  where
    pick i w at d = case anim of
      Idle   -> i
      Walk   -> w
      Attack -> at
      Die    -> d

-- Boss vẽ to hơn quái thường
spriteScale :: SpriteSet -> Float
spriteScale DemonSet = 3
spriteScale _        = 2

renderNetWorld :: NetWorld -> Picture
renderNetWorld world = case GS.phase (nwState world) of
  GS.Lobby -> lobbyText
  GS.Playing ms -> Pictures
    ( map (drawEntity assets) (deadFirst (Map.elems (nwMonsters world)))
   ++ map (drawEntity assets) (Map.elems (nwPlayers world))
   ++ map drawBullet (GS.bullets ms)
   ++ [drawPortal (GS.portal ms)]
    )
  GS.RoomResult GS.Victory -> resultText "CHIEN THANG" green
  GS.RoomResult GS.Defeat  -> resultText "THUA CUOC" red
  where
    assets = nwAssets world
    -- xác vẽ trước để không che quái còn sống
    deadFirst es = filter (not . eAlive) es ++ filter eAlive es
    lobbyText = Translate (-300) 0 $ Scale 0.2 0.2 $ Color white $ Text
      ("Phong: " ++ show (length (GS.profiles (nwState world)))
        ++ "/4  -  1: Warrior   2: Archer   Enter: bat dau")
    resultText label col = Scale 0.4 0.4 $ Color col $ Text label

drawEntity :: GameAssets -> Entity -> Picture
drawEntity assets e =
  let (x, y) = toScreenPosition (ePos e)
      s = spriteScale (eSet e)
      sx = if eFacing e then s else negate s
      pic = currentFrame (framesOf assets (eSet e) (animKind (eAnim e))) (eAnim e)
  in Translate x y $ Scale sx s pic

drawBullet :: GS.Bullet -> Picture
drawBullet b =
  let (x, y) = toScreenPosition (GS.bPosition b)
  in Translate x y $ Color yellow $ circleSolid 4

drawPortal :: GS.Portal -> Picture
drawPortal pt =
  let (x, y) = toScreenPosition (GS.portalPos pt)
      col = if GS.portalActive pt then green else greyN 0.4
  in Translate x y $ Color col $ thickCircle 20 3
