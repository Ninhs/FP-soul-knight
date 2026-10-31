module ClientGameplay
  ( stepLocalGameplay
  , movePreviewPlayer
  ) where

import Data.List (find, partition)

import Animation
import Assets (GameAssets)
import qualified Logic.Combat as Combat
import qualified Logic.Movement as Movement
import ClientUI (toEnemyData, toScreenPosition, toWorldPosition, worldScale)
import qualified MapLayout as Map
import Sprites
import Types
import qualified Types.GameState as GS

stepLocalGameplay
  :: Float
  -> (Float, Float)
  -> Bool
  -> ClientState
  -> ClientState
stepLocalGameplay deltaTime direction attackPressed state
  | appView state /= InGameView = state
  | otherwise = case GS.players currentMap of
      [] -> state
      player : _ -> updateFrom player
  where
    currentMap = localMapState state
    dt = max 0 deltaTime
    assets = appAssets state
    playerKind = playerSet (selectedChar state)
    size = selectedMapSize state
    opened = Map.openLinks (clearedRooms state)

    updateFrom player =
      let nextPlayerPosition =
            movePreviewPlayer dt (Map.isBlocked size opened Map.playerHitRadius)
              direction (playerPosition state) (playerMoveSpeed state)
          nextCurrentRoom = case Map.roomAt size nextPlayerPosition of
            Just room -> Just room
            Nothing -> currentRoom state
          nextActivatedRooms = case nextCurrentRoom of
            Just room | room `notElem` activatedRooms state ->
              activatedRooms state ++ [room]
            _ -> activatedRooms state
          movedPlayer = player { GS.position = toWorldPosition nextPlayerPosition }
          cooledAttack = max 0 (localAttackCooldown state - dt)
          (afterPlayerAttack, playerBullets, nextAttackCooldown, effectPosition, effectStarted) =
            applyPlayerAttack attackPressed cooledAttack nextCurrentRoom movedPlayer
              (GS.monsters currentMap) (GS.bullets currentMap)
          cooledMonsterCooldowns =
            [ (monsterId, max 0 (cooldown - dt))
            | (monsterId, cooldown) <- localMonsterAttackCooldowns state
            ]
          (afterMonsterAI, attackedPlayer, enemyBullets, nextMonsterCooldowns) =
            stepMonsters size opened dt nextCurrentRoom nextPlayerPosition
              cooledMonsterCooldowns
              afterPlayerAttack movedPlayer playerBullets
          steppedBullets =
            Combat.stepBullets
              (Map.isBlocked size opened 0 . toScreenPosition) dt enemyBullets
          (bulletsAfterPlayerHits, monstersAfterHits) =
            Combat.resolvePlayerBullets steppedBullets afterMonsterAI
          (remainingBullets, playersAfterHits) =
            Combat.resolveMonsterBullets bulletsAfterPlayerHits [attackedPlayer]
          finalPlayer = case playersAfterHits of
            [] -> attackedPlayer
            updated : _ -> updated
          (livingMonsters, deadMonsters) = Combat.splitDeadMonsters monstersAfterHits
          newDrops = concatMap monsterDrops deadMonsters
          allDrops = GS.drops currentMap ++ newDrops
          (picked, leftDrops) = partition (isPicked finalPlayer) allDrops
          goldGain = sum [ g | d <- picked, GS.GoldDrop g <- [GS.dType d] ]
          expGain  = sum [ e | d <- picked, GS.ExpDrop e  <- [GS.dType d] ]
          (nextLevel, nextExp) =
            levelUp (playerLevel state) (playerExperience state + expGain)
          bossDied = any ((== 900) . GS.monsterId) deadMonsters
          nextBossDefeated = bossDefeated state || bossDied
          newlyCleared =
            [ room
            | room <- [Map.NormalA, Map.NormalB]
            , room `elem` nextActivatedRooms
            , maybe False (not . null) (lookup room (Map.monsterSpawns size))
            , room `notElem` clearedRooms state
            , not (any ((== Just room) . Map.homeRoomOfMonster . GS.monsterId)
                livingMonsters)
            ]
          nextClearedRooms = clearedRooms state ++ newlyCleared
          nextMap = currentMap
            { GS.players = [finalPlayer]
            , GS.monsters = livingMonsters
            , GS.bullets = remainingBullets
            , GS.drops = leftDrops
            }
          playerAlive = Combat.isPlayerAlive finalPlayer
          nextView
            | not playerAlive = DefeatView
            | nextBossDefeated = VictoryView
            | otherwise = InGameView
          nextEffectTimer
            | effectStarted = 0.18
            | otherwise = max 0 (attackEffectTimer state - dt)
          nextEffectPosition
            | effectStarted = effectPosition
            | otherwise = attackEffectPos state

          -- Animation người chơi
          playerMoved = nextPlayerPosition /= playerPosition state
          nextPlayerFacing =
            facingAfter (fst nextPlayerPosition) (fst (playerPosition state)) (playerFacingRight state)
          nextPlayerAnim =
            stepAnim dt (advanceAnim assets playerKind effectStarted playerMoved (playerAnim state))

          -- Animation quái: các quái vừa đánh trong tick này
          attackedIds =
            [ mid
            | (mid, after) <- nextMonsterCooldowns
            , after > 0
            , maybe True (<= 0) (lookup mid cooledMonsterCooldowns)
            ]
          nextEnemies = map (advanceEnemy assets dt attackedIds (enemies state)) livingMonsters
          -- Xác quái giữ lại và chạy hết animation Die
          nextCorpses =
            map (\e -> e { enemyAnim = stepAnim dt (enemyAnim e) }) (enemyCorpses state)
              ++ map (killedEnemy dt) deadMonsters
          nextBossAnim = stepAnim dt (bossAnim state)
          boss = find ((== GS.Boss) . GS.kind) livingMonsters
      in state
          { localMapState = nextMap
          , localAttackCooldown = nextAttackCooldown
          , localMonsterAttackCooldowns =
              filter (\(id', _) -> id' `elem` map GS.monsterId livingMonsters)
                nextMonsterCooldowns
          , playerPosition = nextPlayerPosition
          , playerPos = nextPlayerPosition
          , currentRoom = nextCurrentRoom
          , activatedRooms = nextActivatedRooms
          , clearedRooms = nextClearedRooms
          , bossDefeated = nextBossDefeated
          , playerHealth = fromIntegral (GS.hp (GS.stats finalPlayer))
          , goldCoins = goldCoins state + goldGain
          , playerLevel = nextLevel
          , playerExperience = nextExp
          , enemies = nextEnemies
          , enemyCorpses = nextCorpses
          , projectiles =
              [ ProjectileData (toScreenPosition (GS.bPosition bullet))
              | bullet <- remainingBullets
              ]
          , attackEffectPos = nextEffectPosition
          , attackEffectTimer = nextEffectTimer
          , appView = nextView
          , statusText =
              if nextView == DefeatView then "DEFEAT"
              else if nextView == VictoryView then "ROOM CLEARED"
              else if not (null newlyCleared) then "Room cleared! Door opened."
              else statusText state
          , bossHealth = maybe 0 (fromIntegral . GS.hp . GS.mStats) boss
          , bossMaxHealth = maybe 0 (fromIntegral . GS.maxHp . GS.mStats) boss
          , isVictory = nextView == VictoryView
          , playerAnim = nextPlayerAnim
          , playerFacingRight = nextPlayerFacing
          , bossAnim = nextBossAnim
          }

-- Chọn trạng thái animation theo thứ tự: đánh > đi > đứng yên.
-- Đòn đánh đang chạy dở thì giữ nguyên cho đến khi hết.
advanceAnim :: GameAssets -> SpriteSet -> Bool -> Bool -> AnimState -> AnimState
advanceAnim assets set started moved st
  | started = startAnim Attack
  | animKind st == Attack && not (animFinished (framesOf assets set Attack) st) = st
  | moved = setAnimKind Walk st
  | otherwise = setAnimKind Idle st

advanceEnemy :: GameAssets -> Float -> [Int] -> [EnemyData] -> GS.Monster -> EnemyData
advanceEnemy assets dt attackedIds previous monster =
  let base = toEnemyData monster
      (prevAnim, prevPos, prevFacing) =
        case find ((== GS.monsterId monster) . enemyId) previous of
          Just e -> (enemyAnim e, enemyPos e, enemyFacingRight e)
          Nothing -> (startAnim Idle, enemyPos base, enemyFacingRight base)
      newPos = enemyPos base
      moved = newPos /= prevPos
      facing = facingAfter (fst newPos) (fst prevPos) prevFacing
      started = GS.monsterId monster `elem` attackedIds
      anim = stepAnim dt
        (advanceAnim assets (enemySprite base) started moved prevAnim)
  in base { enemyAnim = anim, enemyFacingRight = facing }

killedEnemy :: Float -> GS.Monster -> EnemyData
killedEnemy dt monster =
  let base = toEnemyData monster
  in base
      { enemyIsAlive = False
      , enemyAnim = stepAnim dt (startAnim Die)
      }

facingAfter :: Float -> Float -> Bool -> Bool
facingAfter now before old
  | now > before = True
  | now < before = False
  | otherwise = old

applyPlayerAttack
  :: Bool
  -> Float
  -> Maybe Map.RoomId
  -> GS.Player
  -> [GS.Monster]
  -> [GS.Bullet]
  -> ([GS.Monster], [GS.Bullet], Float, (Float, Float), Bool)
applyPlayerAttack attackPressed cooldown room player monsters bullets
  | room == Nothing = (monsters, bullets, cooldown, (0, 0), False)
  | not attackPressed || cooldown > 0 =
      (monsters, bullets, cooldown, (0, 0), False)
  | otherwise = case Combat.selectAutoTarget player roomMonsters of
      Nothing -> (monsters, bullets, 0, (0, 0), False)
      Just target ->
        let nextCooldown = max 0.1 (GS.atkSpeed (GS.stats player))
            effectPosition = toScreenPosition (GS.mPosition target)
        in case Combat.playerAttack player target of
            Combat.MeleeHit damaged ->
              ( replaceMonster damaged monsters
              , bullets
              , nextCooldown
              , effectPosition
              , True
              )
            Combat.Shot bullet ->
              ( monsters
              , bullets ++ [bullet]
              , nextCooldown
              , effectPosition
              , True
              )
  where
    roomMonsters =
      [ monster
      | monster <- monsters
      , Map.homeRoomOfMonster (GS.monsterId monster) == room
      ]

stepMonsters
  :: Map.MapSize
  -> [Map.Link]
  -> Float
  -> Maybe Map.RoomId
  -> (Float, Float)
  -> [(Int, Float)]
  -> [GS.Monster]
  -> GS.Player
  -> [GS.Bullet]
  -> ([GS.Monster], GS.Player, [GS.Bullet], [(Int, Float)])
stepMonsters size opened dt playerRoom playerPositionPx cooldowns monsters player bullets =
  foldl step ([], player, bullets, []) monsters
  where
    step (updated, currentPlayer, currentBullets, updatedCooldowns) monster
      | not (Combat.isMonsterAlive monster) =
          (monster : updated, currentPlayer, currentBullets, updatedCooldowns)
      | playerRoom == Nothing || Map.homeRoomOfMonster monsterId /= playerRoom =
          (monster : updated, currentPlayer, currentBullets,
            (monsterId, cooldown) : updatedCooldowns)
      | gap > aggroRange =
          (monster : updated, currentPlayer, currentBullets,
            (monsterId, cooldown) : updatedCooldowns)
      | gap <= attackRange && cooldown <= 0 =
          case Combat.monsterAttack monster currentPlayer of
            Combat.MonsterMeleeHit damagedPlayer ->
              ( monster : updated
              , damagedPlayer
              , currentBullets
              , (monsterId, attackInterval) : updatedCooldowns
              )
            Combat.MonsterShot bullet ->
              ( monster : updated
              , currentPlayer
              , currentBullets ++ [bullet]
              , (monsterId, attackInterval) : updatedCooldowns
              )
      | gap > attackRange =
          let targetPosition =
                Movement.moveTowards monsterPositionPx playerPositionPx
                  (monsterSpeed monster * worldScale * dt)
              delta = (fst targetPosition - fst monsterPositionPx,
                       snd targetPosition - snd monsterPositionPx)
              home = Map.homeRoomOfMonster monsterId
              blocked position =
                Map.isBlocked size opened Map.monsterHitRadius position
                  || maybe True
                    (\room -> not (Map.geomRoomContains size room
                      Map.monsterHitRadius position))
                    home
              advancedPosition =
                toWorldPosition
                  (Movement.moveWithAxisCollision blocked monsterPositionPx delta)
          in ( monster { GS.mPosition = advancedPosition } : updated
             , currentPlayer
             , currentBullets
             , (monsterId, cooldown) : updatedCooldowns
             )
      | otherwise =
          (monster : updated, currentPlayer, currentBullets,
            (monsterId, cooldown) : updatedCooldowns)
      where
        monsterId = GS.monsterId monster
        monsterPositionPx = toScreenPosition (GS.mPosition monster)
        gap = Movement.distance monsterPositionPx playerPositionPx
        cooldown = lookupCooldown monsterId cooldowns
        attackRange = worldScale * case GS.kind monster of
          GS.Regular GS.Ranged _ -> 4.5
          _ -> 1.1
        attackInterval = max 0.25 (GS.atkSpeed (GS.mStats monster))
        aggroRange = 12 * worldScale

monsterSpeed :: GS.Monster -> Float
monsterSpeed = max 0 . GS.moveSpeed . GS.mStats

lookupCooldown :: Int -> [(Int, Float)] -> Float
lookupCooldown monsterId cooldowns =
  maybe 0 id (lookup monsterId cooldowns)

replaceMonster :: GS.Monster -> [GS.Monster] -> [GS.Monster]
replaceMonster replacement = map update
  where
    update monster
      | GS.monsterId monster == GS.monsterId replacement = replacement
      | otherwise = monster

movePreviewPlayer
  :: Float
  -> ((Float, Float) -> Bool)
  -> (Float, Float)
  -> (Float, Float)
  -> Float
  -> (Float, Float)
movePreviewPlayer deltaTime blocked (dx, dy) position speed =
  Movement.moveWithAxisCollision blocked position delta
  where
    delta = Movement.stepFromInput (dx, dy) speed (max 0 deltaTime)

-- Phần thưởng theo loại quái: (vàng, kinh nghiệm)
monsterReward :: GS.Monster -> (Int, Int)
monsterReward m = case GS.kind m of
  GS.Boss                -> (200, 300)
  GS.Regular GS.Ranged _ -> (15, 25)
  GS.Regular GS.Melee _  -> (10, 20)

-- Quái chết thì rơi một cục vàng và một cục kinh nghiệm tại chỗ
monsterDrops :: GS.Monster -> [GS.Drop]
monsterDrops m =
  let (gold, exp') = monsterReward m
      p = GS.mPosition m
  in [GS.Drop p (GS.GoldDrop gold), GS.Drop p (GS.ExpDrop exp')]

-- Người chơi nhặt khi ở đủ gần (đơn vị world, 1 ≈ 35 px)
pickupRadius :: Float
pickupRadius = 1.0

isPicked :: GS.Player -> GS.Drop -> Bool
isPicked p d = Movement.distance (GS.position p) (GS.dPosition d) <= pickupRadius

-- Lên cấp khi đủ kinh nghiệm: ngưỡng = cấp × 100 (khớp HUD)
levelUp :: Int -> Int -> (Int, Int)
levelUp lv xp
  | xp >= lv * 100 = levelUp (lv + 1) (xp - lv * 100)
  | otherwise      = (lv, xp)