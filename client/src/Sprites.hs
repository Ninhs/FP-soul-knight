module Sprites
  ( framesOf
  , playerSet
  ) where

import Graphics.Gloss (Picture)
import Animation (AnimKind(..), SpriteSet(..))
import Assets (GameAssets(..))
import Types (Character(..))

playerSet :: Character -> SpriteSet
playerSet Swordsman = SoldierSet
playerSet Knight    = SoldierSet
playerSet Archer    = ArcherSet
playerSet Bomber    = ArcherSet
playerSet Mage      = ArcherSet

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