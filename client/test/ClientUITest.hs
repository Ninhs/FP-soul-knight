module Main where

import Assets (emptyGameAssets)
import ClientUI
import ClientGameplay (movePreviewPlayer, stepLocalGameplay)
import Types
import qualified MapLayout as Map
import qualified Logic.Combat as Combat
import qualified Types.GameState as GS

main :: IO ()
main = do
  let initial = initialClientState emptyGameAssets
  characterFlow initial
  mapLayoutFlow
  gameplayFlow initial
  shopFlow initial
  upgradeFlow initial
  lobbyValidation initial
  assert "main-menu exit is an explicit UI effect"
    (isQuit (reduceUi (UiClick (-420, -195)) initial))
  putStrLn "Client UI reducer tests passed."

characterFlow :: ClientState -> IO ()
characterFlow initial = do
  selected <- expectHandled "start opens character select"
    (reduceUi (UiClick (-420, 155)) initial)
  assert "character-select screen is active"
    (appView selected == CharacterSelectView)
  archer <- expectHandled "archer card selects archer"
    (reduceUi (UiClick (-225, -75)) selected)
  assert "archer selection updates selected character"
    (selectedChar archer == Archer)
  mapScreen <- expectHandled "enter advances to map selection"
    (reduceUi UiEnter archer)
  assert "map-select screen is active" (appView mapScreen == MapSelectView)
  playing <- expectHandled "solo button starts local preview"
    (reduceUi (UiClick (40, -285)) mapScreen)
  assert "local preview is active" (appView playing == InGameView)
  paused <- expectHandled "pause button opens pause overlay"
    (reduceUi (UiClick (560, 320)) playing)
  assert "paused screen is active" (appView paused == PausedView)
  resumed <- expectHandled "enter resumes the preview"
    (reduceUi UiEnter paused)
  assert "preview resumes" (appView resumed == InGameView)
  assert "world click remains unhandled for combat input"
    (isUnhandled (reduceUi (UiClick (0, 0)) resumed))

gameplayFlow :: ClientState -> IO ()
gameplayFlow initial = do
  selected <- expectHandled "start local run for gameplay checks"
    (reduceUi UiEnter initial)
  mapScreen <- expectHandled "advance to map for gameplay checks"
    (reduceUi UiEnter selected)
  playing <- expectHandled "start spawned local run"
    (reduceUi (UiClick (40, -285)) mapScreen)
  let startingMap = localMapState playing
      startingMonsters = GS.monsters startingMap
      size = selectedMapSize playing
  assert "local preview spawns five A monsters, six B monsters, and one boss"
    (length startingMonsters == 12)
  assert "all spawned monsters start alive"
    (all (Combat.isMonsterAlive) startingMonsters)
  assert "all room monster identifiers are unique"
    (unique (map GS.monsterId startingMonsters))
  assert "local run starts in the center of the start room"
    (playerPosition playing == Map.startPosition size
      && Map.roomAt size (playerPosition playing) == Just Map.StartRoom
      && null [monster | monster <- startingMonsters,
        Map.homeRoomOfMonster (GS.monsterId monster) == Just Map.StartRoom])
  assert "only the demon has boss kind and it belongs to the boss room"
    (all (\monster -> (GS.kind monster == GS.Boss)
        == (Map.homeRoomOfMonster (GS.monsterId monster) == Just Map.BossRoom))
      startingMonsters)
  let idleTick = stepLocalGameplay 0.25 (0, 0) False playing
  assert "monsters in other rooms stay idle and do not move"
    (all (\monster -> maybe False
        ((== GS.mPosition monster) . GS.mPosition)
        (findMonster (GS.monsterId monster) (GS.monsters (localMapState idleTick))))
      startingMonsters)
  let attackFromStart = stepLocalGameplay 0 (0, 0) True playing
  assert "player auto-target ignores monsters in other rooms"
    (all (\monster -> maybe False
        ((== GS.hp (GS.mStats monster)) . GS.hp . GS.mStats)
        (findMonster (GS.monsterId monster) (GS.monsters (localMapState attackFromStart))))
      startingMonsters)
  let moved = stepLocalGameplay 0.1 (1, 0) False playing
  assert "held movement input advances the player"
    (fst (playerPosition moved) > fst (playerPosition playing))

  firstBefore <- requireMonster 101 startingMonsters
  let firstPosition = toScreenPosition (GS.mPosition firstBefore)
      chasePosition = (fst firstPosition - 75, snd firstPosition)
      nearTarget = (setPlayerAtPixel chasePosition playing)
        { currentRoom = Just Map.NormalA
        , activatedRooms = [Map.NormalA]
        }
      chased = stepLocalGameplay 0.25 (0, 0) False nearTarget
      boundaryPosition = (-100, 192)
      boundaryMonster = firstBefore
        { GS.mPosition = toWorldPosition boundaryPosition }
      boundaryState = (setPlayerAtPixel (-20, 192) playing)
        { currentRoom = Just Map.NormalA
        , localMapState = startingMap
            { GS.monsters = map
                (\monster -> if GS.monsterId monster == 101
                  then boundaryMonster
                  else monster)
                startingMonsters
            }
        }
      boundaryStep = stepLocalGameplay 1 (0, 0) False boundaryState
      movedBoundaryMonster = findMonster 101
        (GS.monsters (localMapState boundaryStep))
  assert "monsters remain inside their home room even at an open doorway"
    (maybe False
      (\monster -> Map.geomRoomContains size Map.NormalA Map.monsterHitRadius
          (toScreenPosition (GS.mPosition monster)))
      movedBoundaryMonster)
  firstAfter <- requireMonster (GS.monsterId firstBefore)
    (GS.monsters (localMapState chased))
  chasedPlayer <- requirePlayer (GS.players (localMapState chased))
  let distanceAfter = distance (GS.position chasedPlayer) (GS.mPosition firstAfter)
  assert "nearby monster advances toward the player"
    (distanceAfter < distance (toWorldPosition chasePosition)
      (GS.mPosition firstBefore))

  let attackPosition = (fst firstPosition - 20, snd firstPosition)
      attackingState = (setPlayerAtPixel attackPosition nearTarget)
        { localMonsterAttackCooldowns = (101, 0)
            : filter ((/= 101) . fst) (localMonsterAttackCooldowns nearTarget)
        }
      attacked = stepLocalGameplay 0.1 (0, 0) True attackingState
  attackedMonster <- requireMonster 101 (GS.monsters (localMapState attacked))
  assert "monsterAttack from the shared combat module damages the player"
    (playerHealth attacked < playerHealth attackingState)
  let attackedAgain = stepLocalGameplay 0.1 (0, 0) True attacked
  monsterAfterCooldownProbe <- requireMonster 101
    (GS.monsters (localMapState attackedAgain))
  assert "Combat.selectAutoTarget applies shared combat damage"
    (GS.hp (GS.mStats attackedMonster) < GS.hp (GS.mStats firstBefore))
  assert "attack cooldown prevents a second immediate hit"
    (GS.hp (GS.mStats monsterAfterCooldownProbe) == GS.hp (GS.mStats attackedMonster))
  let cooled = stepLocalGameplay 0.5 (0, 0) False attacked
      killed = stepLocalGameplay 0.1 (0, 0) True cooled
      monstersAfterKill = GS.monsters (localMapState killed)
  assert "dead monster is removed from the active map"
    (length monstersAfterKill == 11
      && all ((/= 101) . GS.monsterId) monstersAfterKill)

  let unvisitedClearCandidate = playing
        { localMapState = startingMap
            { GS.monsters = filter
                ((/= Just Map.NormalA) . Map.homeRoomOfMonster . GS.monsterId)
                startingMonsters
            }
        }
      unvisitedStep = stepLocalGameplay 0 (0, 0) False unvisitedClearCandidate
      activeWithEnemies =
        setPlayerAtPixel (Map.roomCenter size Map.NormalA) playing
      activeStep = stepLocalGameplay 0 (0, 0) False activeWithEnemies
      clearedAState =
        setPlayerAtPixel (Map.roomCenter size Map.NormalA)
          (playing
          { localMapState = startingMap
              { GS.monsters = filter
                  ((/= Just Map.NormalA) . Map.homeRoomOfMonster . GS.monsterId)
                  startingMonsters
              }
          })
      clearedA = stepLocalGameplay 0 (0, 0) False clearedAState
  assert "a room cannot clear before activation"
    (Map.NormalA `notElem` clearedRooms unvisitedStep)
  assert "an activated room with living monsters remains uncleared"
    (Map.NormalA `elem` activatedRooms activeStep
      && Map.NormalA `notElem` clearedRooms activeStep)
  assert "entering a cleared room activates and clears it exactly once"
    (Map.NormalA `elem` activatedRooms clearedA
      && Map.NormalA `elem` clearedRooms clearedA
      && Map.LinkTLtoTR `elem` Map.openLinks (clearedRooms clearedA))
  let clearedAAgain = stepLocalGameplay 0 (0, 0) False clearedA
  assert "cleared room state is persistent and contains no duplicates"
    (clearedRooms clearedAAgain == [Map.NormalA])
  assert "clearing A alone does not open the boss door"
    (Map.LinkTRtoBR `notElem` Map.openLinks (clearedRooms clearedA))

  boss <- requireMonster 900 startingMonsters
  let normalRoomsCleared = stepLocalGameplay 0 (0, 0) False
        ((setPlayerAtPixel (Map.roomCenter size Map.NormalB) playing)
          { currentRoom = Just Map.NormalB
          , clearedRooms = [Map.NormalA, Map.NormalB]
          , activatedRooms = [Map.NormalA, Map.NormalB]
          , localMapState = startingMap
              { GS.monsters = filter
                  ((== Just Map.BossRoom) . Map.homeRoomOfMonster . GS.monsterId)
                  startingMonsters
              }
          })
  assert "clearing both normal rooms does not win before the boss dies"
    (appView normalRoomsCleared == InGameView
      && not (bossDefeated normalRoomsCleared))
  let
      oneHpBoss = boss
        { GS.mStats = (GS.mStats boss) { GS.hp = 1, GS.maxHp = 1 } }
      bossPosition = toScreenPosition (GS.mPosition boss)
      bossFight = (setPlayerAtPixel bossPosition playing)
        { currentRoom = Just Map.BossRoom
        , localMapState = startingMap
            { GS.monsters = map
                (\monster -> if GS.monsterId monster == 900
                  then oneHpBoss
                  else monster)
                startingMonsters
            }
        }
      won = stepLocalGameplay 0 (0, 0) True bossFight
  assert "killing the boss while alive wins the run"
    (bossDefeated won && appView won == VictoryView
      && statusText won == "ROOM CLEARED")

  let simultaneousDeath = bossFight
        { localMapState = (localMapState bossFight)
            { GS.players = map
                (\player -> player
                  { GS.stats = (GS.stats player) { GS.hp = 1 } })
                (GS.players (localMapState bossFight))
            , GS.bullets =
                [ GS.Bullet
                    { GS.bPosition = toWorldPosition bossPosition
                    , GS.bVelocity = (0, 0)
                    , GS.bOwner = -901
                    , GS.bDamage = 10
                    , GS.bLife = 1
                    }
                ]
            }
        , playerHealth = 1
        }
      lost = stepLocalGameplay 0 (0, 0) True simultaneousDeath
  assert "defeat takes precedence when player and boss die in one tick"
    (bossDefeated lost && appView lost == DefeatView
      && statusText lost == "DEFEAT")

mapLayoutFlow :: IO ()
mapLayoutFlow = do
  let size = LargeMap
      (doorX, _) = Map.roomCenter size Map.StartRoom
      (_, verticalDoorY) = Map.roomCenter size Map.NormalA
      sizes = [SmallMap, MediumMap, LargeMap]
      initial = initialClientState emptyGameAssets
  assert "start position is classified as StartRoom"
    (Map.roomAt size (Map.startPosition size) == Just Map.StartRoom)
  assert "room boundaries classify into the upper/right room"
    (Map.roomAt size (doorX, 0) == Just Map.NormalA
      && Map.roomAt size (0, verticalDoorY) == Just Map.NormalB
      && Map.roomAt size (200, -100) == Just Map.BossRoom
      && Map.roomAt size (-100, -100) == Just Map.StartRoom)
  assert "all four rooms have equal dimensions"
    (let rooms = map (Map.roomRect size)
          [Map.StartRoom, Map.NormalA, Map.NormalB, Map.BossRoom]
     in case rooms of
       firstRoom : otherRooms ->
         all (\room -> Map.rectWidth room == Map.rectWidth firstRoom
              && Map.rectHeight room == Map.rectHeight firstRoom) otherRooms
       [] -> False)
  assert "outer walls align with the map image bounds"
    (all (\mapSize ->
        let halfWidth = Map.mapWidth mapSize / 2
            halfHeight = Map.mapHeight mapSize / 2
        in case Map.permanentWalls mapSize of
          leftWall : rightWall : bottomWall : topWall : _ ->
            Map.rectX leftWall == negate halfWidth
              && Map.rectX rightWall + Map.rectWidth rightWall == halfWidth
              && Map.rectY bottomWall == negate halfHeight
              && Map.rectY topWall + Map.rectHeight topWall == halfHeight
          _ -> False)
      sizes)
  assert "starting BL-to-TL link is the only initial open link"
    (Map.openLinks [] == [Map.LinkBLtoTL])
  assert "A and B clear independently unlock their next links"
    (Map.LinkTLtoTR `elem` Map.openLinks [Map.NormalA]
      && Map.LinkTRtoBR `elem` Map.openLinks [Map.NormalB]
      && Map.LinkTRtoBR `notElem` Map.openLinks [Map.NormalA])
  assert "all permanent and closed-door wall rectangles have positive dimensions"
    (all (\mapSize ->
        all positive (Map.permanentWalls mapSize ++ Map.closedDoorRects mapSize []))
      sizes)
  assert "permanent wall rectangles do not overlap"
    (all (\mapSize ->
        all (uncurry disjoint) (pairs (Map.permanentWalls mapSize)))
      sizes)
  assert "door collision blocks movement until the link opens"
    (let closed = movePreviewPlayer 0.5
            (Map.isBlocked SmallMap [Map.LinkBLtoTL] Map.playerHitRadius)
            (1, 0) (-40, 192) 100
         opened = movePreviewPlayer 0.5
            (Map.isBlocked SmallMap [Map.LinkBLtoTL, Map.LinkTLtoTR]
              Map.playerHitRadius)
            (1, 0) (-40, 192) 100
     in closed == (-40, 192) && opened == (10, 192))
  assert "bullets are blocked by closed doors and pass through open ones"
    (Map.isBlocked SmallMap [Map.LinkBLtoTL] 0 (0, 192)
      && not (Map.isBlocked SmallMap
        [Map.LinkBLtoTL, Map.LinkTLtoTR] 0 (0, 192)))
  assert "player collision blocks permanent boundary walls"
    (movePreviewPlayer 1
      (Map.isBlocked SmallMap [Map.LinkBLtoTL] Map.playerHitRadius)
      (0, 1) (-200, 300) 100 == (-200, 300))
  assert "all generated spawns stay in their room and away from doors"
    (all (\mapSize -> all (validSpawn mapSize) (spawnsWithPosition mapSize)) sizes)
  assert "every monster ID maps to its configured home room"
    (all (\(room, index, _) ->
        Map.homeRoomOfMonster (monsterIdFor room index) == Just room)
      (spawnsWithPosition size))
  assert "initial local player uses the start position in screen and world units"
    (case GS.players (localMapState initial) of
      player : _ ->
        playerPosition initial == Map.startPosition (selectedMapSize initial)
          && GS.position player ==
            toWorldPosition (Map.startPosition (selectedMapSize initial))
      [] -> False)
  where
    positive rect = Map.rectWidth rect > 0 && Map.rectHeight rect > 0
    disjoint first second =
      Map.rectX first + Map.rectWidth first <= Map.rectX second + tolerance
        || Map.rectX second + Map.rectWidth second <= Map.rectX first + tolerance
        || Map.rectY first + Map.rectHeight first <= Map.rectY second + tolerance
        || Map.rectY second + Map.rectHeight second <= Map.rectY first + tolerance
      where
        tolerance = 0.01
    pairs [] = []
    pairs (first : rest) = [(first, other) | other <- rest] ++ pairs rest
    validSpawn mapSize (room, _, position) =
      Map.geomRoomContains mapSize room (Map.monsterHitRadius + Map.spawnMargin) position
        && all (\link -> pointRectDistance position (Map.linkDoor mapSize link)
            >= Map.monsterHitRadius)
          [Map.LinkBLtoTL, Map.LinkTLtoTR, Map.LinkTRtoBR]
    monsterIdFor room index = case room of
      Map.NormalA -> 100 + index
      Map.NormalB -> 200 + index
      Map.BossRoom -> 900
      Map.StartRoom -> 0

spawnsWithPosition :: Map.MapSize -> [(Map.RoomId, Int, (Float, Float))]
spawnsWithPosition size =
  [ (room, index, (centerX + fx * halfWidth, centerY + fy * halfHeight))
  | (room, spawns) <- Map.monsterSpawns size
  , (index, spawn) <- zip [1 ..] spawns
  , let (fx, fy) = Map.spawnFrac spawn
  , let (centerX, centerY) = Map.roomCenter size room
  , let (halfWidth, halfHeight) = Map.roomHalfSize size room
  ]

pointRectDistance :: (Float, Float) -> Map.Rect -> Float
pointRectDistance (x, y) rect =
  sqrt ((x - nearestX) * (x - nearestX) + (y - nearestY) * (y - nearestY))
  where
    nearestX = max (Map.rectX rect)
      (min (Map.rectX rect + Map.rectWidth rect) x)
    nearestY = max (Map.rectY rect)
      (min (Map.rectY rect + Map.rectHeight rect) y)

findMonster :: Int -> [GS.Monster] -> Maybe GS.Monster
findMonster _ [] = Nothing
findMonster monsterId (monster : rest)
  | GS.monsterId monster == monsterId = Just monster
  | otherwise = findMonster monsterId rest

setPlayerAtPixel :: (Float, Float) -> ClientState -> ClientState
setPlayerAtPixel position state =
  let mapState = localMapState state
  in state
      { playerPosition = position
      , playerPos = position
      , localMapState = mapState
          { GS.players =
              [player { GS.position = toWorldPosition position }
              | player <- GS.players mapState]
          }
      }

unique :: Eq a => [a] -> Bool
unique [] = True
unique (first : rest) = first `notElem` rest && unique rest

shopFlow :: ClientState -> IO ()
shopFlow initial = do
  shop <- expectHandled "shop menu opens"
    (reduceUi (UiClick (-420, 15)) initial)
  purchased <- expectHandled "shop purchase is handled"
    (reduceUi (UiClick (-270, -100)) shop)
  assert "purchase deducts demo gold" (goldCoins purchased == goldCoins initial - 100)
  assert "purchase enters preview inventory"
    (shopInventory purchased == [(SmallHealthPotion, 1)])
  broke <- expectHandled "shop refresh without gems gives feedback"
    (reduceUi (UiClick (20, -280)) (shop { gems = 0 }))
  assert "insufficient gem message is shown"
    (appView broke == ShopNoticeView && noticeText broke == "Not enough gems.")

upgradeFlow :: ClientState -> IO ()
upgradeFlow initial = do
  upgrades <- expectHandled "upgrade screen opens"
    (reduceUi (UiClick (-420, -55)) initial)
  upgraded <- expectHandled "health upgrade is handled"
    (reduceUi (UiClick (-120, 28)) upgrades)
  assert "health upgrade changes the demo stat"
    (playerMaxHealth upgraded == playerMaxHealth initial + 20)
  assert "health upgrade charges demo gold"
    (goldCoins upgraded == goldCoins initial - 100)

lobbyValidation :: ClientState -> IO ()
lobbyValidation initial = do
  character <- expectHandled "menu opens character select"
    (reduceUi UiEnter initial)
  mapScreen <- expectHandled "map screen opens from character select"
    (reduceUi UiEnter character)
  lobby <- expectHandled "co-op mock lobby opens"
    (reduceUi (UiClick (330, -285)) mapScreen)
  assert "lobby clearly identifies mock mode"
    (appView lobby == LobbyView && not (null (statusText lobby)))
  focused <- expectHandled "room input receives focus"
    (reduceUi (UiClick (-405, 215)) lobby)
  typed <- expectHandled "room input accepts and normalizes text"
    (reduceUi (UiKey 'a') focused)
  assert "room input is uppercase" (roomInput typed == "A")
  let invalid = reduceUi (UiKey '!') typed
  assert "invalid room-code characters are rejected" (isUnhandled invalid)
  submitted <- expectHandled "room-code submit gives local-only feedback"
    (reduceUi (UiClick (215, 215)) typed)
  assert "lobby does not claim a network connection"
    ("not connected" `contains` statusText submitted)

expectHandled :: String -> UiResult -> IO ClientState
expectHandled _ (UiHandled state) = pure state
expectHandled label result = fail (label ++ ": expected a handled UI action, got " ++ showResult result)

showResult :: UiResult -> String
showResult UiUnhandled = "unhandled"
showResult UiQuit = "quit"
showResult (UiHandled _) = "handled"

isQuit :: UiResult -> Bool
isQuit UiQuit = True
isQuit _ = False

isUnhandled :: UiResult -> Bool
isUnhandled UiUnhandled = True
isUnhandled _ = False

assert :: String -> Bool -> IO ()
assert label condition =
  if condition then pure () else fail ("Assertion failed: " ++ label)

contains :: String -> String -> Bool
contains needle haystack = any (startsWith needle) (tails haystack)
  where
    tails [] = [[]]
    tails value@(_ : rest) = value : tails rest
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (first : rest) (actual : remaining) =
      first == actual && startsWith rest remaining

requireMonster :: Int -> [GS.Monster] -> IO GS.Monster
requireMonster targetId monsters =
  case filter ((== targetId) . GS.monsterId) monsters of
    monster : _ -> pure monster
    [] -> fail ("Missing monster id: " ++ show targetId)

requirePlayer :: [GS.Player] -> IO GS.Player
requirePlayer players = case players of
  player : _ -> pure player
  [] -> fail "Local gameplay state did not contain a player."

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
