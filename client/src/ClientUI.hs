module ClientUI
  ( UiAction(..)
  , UiResult(..)
  , reduceUi
  , startLocalRun
  , toEnemyData
  , worldScale
  , toWorldPosition
  , toScreenPosition
  ) where

import Data.Char (isAlphaNum, toUpper)
import Animation (startAnim, AnimKind(..), SpriteSet(..))
import Components.UI (insideRect)
import qualified Logic.Combat as Combat
import qualified Views.Shop as Shop
import qualified Views.Upgrade as Upgrade
import Types
import qualified MapLayout as Map
import qualified Types.GameState as GS

data UiAction
  = UiClick (Float, Float)
  | UiKey Char
  | UiEnter
  deriving (Eq, Show)

data UiResult
  = UiUnhandled
  | UiHandled ClientState
  | UiQuit

reduceUi :: UiAction -> ClientState -> UiResult
reduceUi action state = case action of
  UiClick position -> reduceClick position state
  UiKey key -> maybe UiUnhandled UiHandled (reduceKey key state)
  UiEnter -> maybe UiUnhandled UiHandled (reduceEnter state)

reduceClick :: (Float, Float) -> ClientState -> UiResult
reduceClick position state
  | appView state == MainMenuView
      && insideRect position (-420, -195) 300 54 = UiQuit
  | otherwise = maybe UiUnhandled UiHandled (clickScreen position state)

reduceEnter :: ClientState -> Maybe ClientState
reduceEnter state = case appView state of
  MainMenuView -> Just state { appView = CharacterSelectView }
  CharacterSelectView -> Just state { appView = MapSelectView }
  MapSelectView -> Just (startLocalRun state)
  LobbyView
    | nameInputActive state ->
        Just state { nameInputActive = False, statusText = "Name set." }
    | roomInputActive state ->
        Just (submitRoomCode (state { roomInputActive = False }))
    | otherwise -> Just (startLocalRun state)
  ShopNoticeView -> Just state { appView = noticeReturnView state }
  TutorialView -> Just state { appView = CharacterSelectView }
  VictoryView -> Just (startLocalRun state)
  DefeatView -> Just (startLocalRun state)
  PausedView -> Just state { appView = InGameView }
  _ -> Nothing

reduceKey :: Char -> ClientState -> Maybe ClientState
reduceKey key state
  | appView state == LobbyView && nameInputActive state =
      editNameInput key state
  | appView state == LobbyView && roomInputActive state =
      editRoomInput key state
  | appView state == CharacterSelectView && key `elem` ['a', 'A', 'd', 'D'] =
      Just (selectCharacter nextCharacter state)
  | appView state == InGameView && key `elem` ['r', 'R']
      && localRunFinished state =
      Just (startLocalRun state)
  | appView state `elem` [VictoryView, DefeatView]
      && key `elem` ['r', 'R'] =
      Just (startLocalRun state)
  | appView state == InGameView && key `elem` ['p', 'P'] =
      Just state { appView = PausedView }
  | appView state == InGameView && key `elem` ['i', 'I'] =
      Just state { inventoryVisible = not (inventoryVisible state) }
  | appView state == InGameView && key `elem` ['1', '2', '3', '4'] =
      Just (useInventoryItem (fromEnum key - fromEnum '1') state)
  | appView state == InGameView && key `elem` ['e', 'E'] =
      Just (interactWithChest state)
  | appView state == TutorialView && key == ' ' =
      Just state { appView = CharacterSelectView }
  | key == '\b' = reduceBackspace state
  | otherwise = Nothing
  where
    nextCharacter
      | selectedChar state == Swordsman = Archer
      | otherwise = Swordsman

reduceBackspace :: ClientState -> Maybe ClientState
reduceBackspace state
  | appView state == LobbyView && nameInputActive state =
      Just state { playerName = dropLast (playerName state) }
  | appView state == LobbyView && roomInputActive state =
      Just state { roomInput = dropLast (roomInput state) }
  | otherwise = Nothing

editRoomInput :: Char -> ClientState -> Maybe ClientState
editRoomInput key state
  | key == '\b' = Just state { roomInput = dropLast (roomInput state) }
  | validRoomCharacter key && length (roomInput state) < 12 =
      Just state { roomInput = roomInput state ++ [toUpper key] }
  | otherwise = Nothing

editNameInput :: Char -> ClientState -> Maybe ClientState
editNameInput key state
  | key == '\b' = Just state { playerName = dropLast (playerName state) }
  | (isAlphaNum key || key == '_') && length (playerName state) < 12 =
      Just state { playerName = playerName state ++ [key] }
  | otherwise = Nothing

validRoomCharacter :: Char -> Bool
validRoomCharacter character = isAlphaNum character || character == '-'

clickScreen :: (Float, Float) -> ClientState -> Maybe ClientState
clickScreen position state = case appView state of
  MainMenuView -> clickMainMenu position state
  CharacterSelectView -> clickCharacterSelect position state
  MapSelectView -> clickMapSelect position state
  LobbyView -> clickLobby position state
  TutorialView -> clickTutorial position state
  InGameView -> clickGameHud position state
  ShopView -> clickShop position state
  UpgradeView -> clickUpgrade position state
  PausedView -> clickPaused position state
  ChestRewardView -> clickChestReward position state
  ShopNoticeView -> clickNotice position state
  ConfirmMenuView -> clickConfirmation position state
  VictoryView -> clickResult position state
  DefeatView -> clickResult position state

clickMainMenu :: (Float, Float) -> ClientState -> Maybe ClientState
clickMainMenu position state
  | insideRect position (-420, 155) 300 54
      || insideRect position (-420, 85) 300 54 =
      Just state { appView = CharacterSelectView }
  | insideRect position (-420, 15) 300 54 =
      Just state { appView = ShopView, returnView = MainMenuView }
  | insideRect position (-420, -55) 300 54 =
      Just state { appView = UpgradeView, returnView = MainMenuView }
  | insideRect position (-420, -125) 300 54 =
      Just state { appView = TutorialView }
  | insideRect position (590, 305) 76 40 =
      Just state
        { appView = ShopNoticeView
        , noticeText = "Settings are not implemented yet."
        , noticeReturnView = MainMenuView
        }
  | otherwise = Nothing

clickCharacterSelect :: (Float, Float) -> ClientState -> Maybe ClientState
clickCharacterSelect position state
  | insideRect position (-375, -75) 104 104
      || insideRect position (-300, -190) 180 48 =
      Just (selectCharacter Swordsman state)
  | insideRect position (-225, -75) 104 104
      || insideRect position (-95, -190) 160 48 =
      Just (selectCharacter Archer state)
  | insideRect position (-220, -280) 190 52 =
      Just state { appView = MainMenuView }
  | insideRect position (220, -280) 220 52 =
      Just state { appView = MapSelectView }
  | otherwise = Nothing

clickMapSelect :: (Float, Float) -> ClientState -> Maybe ClientState
clickMapSelect position state
  | insideRect position (-405, -285) 210 52 =
      Just state { appView = CharacterSelectView }
  | insideRect position (40, -285) 180 52 =
      Just (startLocalRun state { isMultiplayer = False })
  | insideRect position (330, -285) 180 52 =
      Just state
        { appView = LobbyView
        , isMultiplayer = True
        , statusText = "Offline mock lobby. Multiplayer is not connected here."
        }
  | otherwise = Nothing

clickLobby :: (Float, Float) -> ClientState -> Maybe ClientState
clickLobby position state
  | insideRect position (-405, 143) 170 32 =
      Just state { statusText = "Room-code copy is unavailable in this window." }
  | insideRect position (0, 172) 300 34 =
      Just state { nameInputActive = True, roomInputActive = False
                 , statusText = "Type your name (letters/digits), then press Enter." }
  | insideRect position (0, 215) 320 58 =
      Just state { roomInputActive = True, nameInputActive = False
                 , statusText = "Enter letters, digits or -." }
  | insideRect position (215, 215) 130 58 =
      Just (submitRoomCode (state { roomInputActive = False }))
  | insideRect position (-450, -270) 200 52 =
      Just state { appView = MapSelectView, isMultiplayer = False }
  | insideRect position (-150, -270) 220 52 =
      Just state
        { isReady = not (isReady state)
        , statusText = if isReady state then "You are not ready." else "Ready (local mock only)."
        }
  | insideRect position (150, -270) 220 52 =
      Just (startLocalRun state)
  | insideRect position (450, -270) 220 52 =
      Just state { appView = CharacterSelectView }
  | otherwise = Nothing

submitRoomCode :: ClientState -> ClientState
submitRoomCode state
  | null (roomInput state) = state { statusText = "Enter a room code first." }
  | all validRoomCharacter (roomInput state) =
      state { statusText = "Room codes are not connected in this local lobby." }
  | otherwise = state { statusText = "Use only letters, digits and hyphens." }

clickTutorial :: (Float, Float) -> ClientState -> Maybe ClientState
clickTutorial position state
  | insideRect position (-145, -235) 230 48 =
      Just state { appView = CharacterSelectView }
  | insideRect position (145, -235) 180 48 =
      Just state { appView = MainMenuView }
  | otherwise = Nothing

clickGameHud :: (Float, Float) -> ClientState -> Maybe ClientState
clickGameHud position state
  | insideRect position (560, 320) 130 42 =
      Just state { appView = PausedView }
  | insideRect position (560, 265) 130 42 =
      Just state { appView = ShopView, returnView = InGameView }
  | insideRect position (560, 210) 130 42 =
      Just state { appView = UpgradeView, returnView = InGameView }
  | otherwise = Nothing

clickShop :: (Float, Float) -> ClientState -> Maybe ClientState
clickShop position state
  | insideRect position (-230, -280) 200 48 =
      Just state { appView = returnView state }
  | insideRect position (20, 240) 190 44 =
      Just state { shopEquipmentTab = False }
  | insideRect position (240, 240) 190 44 =
      Just state { shopEquipmentTab = True }
  | insideRect position (20, -280) 240 48 && gems state >= 5 =
      Just state { gems = gems state - 5, shopRefreshes = shopRefreshes state + 1 }
  | insideRect position (20, -280) 240 48 =
      Just (showNotice "Not enough gems." ShopView state)
  | otherwise = case Shop.shopItemHit state position of
      Nothing -> Nothing
      Just item -> Just (purchaseItem item state)

clickUpgrade :: (Float, Float) -> ClientState -> Maybe ClientState
clickUpgrade position state
  | insideRect position (-300, -270) 210 48 =
      Just state { appView = returnView state }
  | insideRect position (100, -270) 250 48 && gems state >= 5 =
      Just state { gems = gems state - 5, upgradeRefreshes = upgradeRefreshes state + 1 }
  | insideRect position (100, -270) 250 48 =
      Just (showNotice "Not enough gems." UpgradeView state)
  | otherwise = case Upgrade.upgradeHit state position of
      Nothing -> Nothing
      Just upgrade -> Just (purchaseUpgrade upgrade state)

clickPaused :: (Float, Float) -> ClientState -> Maybe ClientState
clickPaused position state
  | insideRect position (0, 20) 240 52 =
      Just state { appView = InGameView }
  | insideRect position (0, -50) 240 52 =
      Just state { appView = ConfirmMenuView, returnView = InGameView }
  | otherwise = Nothing

clickChestReward :: (Float, Float) -> ClientState -> Maybe ClientState
clickChestReward position state
  | insideRect position (0, -220) 220 54 =
      Just $ setPlayerHealth
        (min (playerMaxHealth state) (playerHealth state + 25))
        state
          { appView = returnView state
          , goldCoins = goldCoins state + 120

          }
  | otherwise = Nothing

clickNotice :: (Float, Float) -> ClientState -> Maybe ClientState
clickNotice position state
  | insideRect position (0, -70) 180 48 =
      Just state { appView = noticeReturnView state }
  | otherwise = Nothing

clickConfirmation :: (Float, Float) -> ClientState -> Maybe ClientState
clickConfirmation position state
  | insideRect position (-125, -72) 190 48 =
      Just state { appView = InGameView }
  | insideRect position (125, -72) 190 48 =
      Just state { appView = MainMenuView, isMultiplayer = False }
  | otherwise = Nothing

clickResult :: (Float, Float) -> ClientState -> Maybe ClientState
clickResult position state
  | insideRect position (-145, -130) 250 58 =
      Just (startLocalRun state)
  | insideRect position (155, -130) 210 58 =
      Just state { appView = MainMenuView, isMultiplayer = False }
  | otherwise = Nothing

selectCharacter :: Character -> ClientState -> ClientState
selectCharacter character state =
  let stats = characterStats character
      newMaxHealth = characterMaxHp stats + fromIntegral (healthUpgradeLevel state * 20)
  in state
    { selectedChar = character
    , playerMaxHealth = newMaxHealth
    , playerHealth = min (playerHealth state) newMaxHealth
    , playerDamage = characterDamage stats + fromIntegral (damageUpgradeLevel state * 5)
    , playerMoveSpeed = characterMoveSpeed stats + fromIntegral (speedUpgradeLevel state * 10)
    }

startLocalRun :: ClientState -> ClientState
startLocalRun state =
  let size = selectedMapSize state
      playerStart = Map.startPosition size
      maxHealth = max 1 (round (playerMaxHealth state))
      playerStats = GS.Stats
        { GS.hp = maxHealth
        , GS.maxHp = maxHealth
        , GS.damage = max 0 (round (playerDamage state))
        , GS.moveSpeed = playerMoveSpeed state / worldScale
        , GS.atkSpeed = 0.45
        , GS.atkRange = 3.0
        }
      player = GS.Player
        { GS.playerId = 1
        , GS.position = toWorldPosition playerStart
        , GS.stats = playerStats
        , GS.status = GS.Alive
        , GS.pendingExp = 0
        , GS.pendingGold = 0
        }
      spawns =
        [ (room, index, spawn)
        | (room, roomSpawns) <- Map.monsterSpawns size
        , (index, spawn) <- zip [1 ..] roomSpawns
        ]
      makeMonster (room, index, spawn) =
        let monsterId' = case room of
              Map.NormalA -> 100 + index
              Map.NormalB -> 200 + index
              Map.BossRoom -> 900
              Map.StartRoom -> 0
            (fx, fy) = Map.spawnFrac spawn
            (centerX, centerY) = Map.roomCenter size room
            (halfWidth, halfHeight) = Map.roomHalfSize size room
            pixelPos = (centerX + fx * halfWidth, centerY + fy * halfHeight)
            maximumHp = Map.spawnHp spawn
            monsterKind = Map.spawnKind spawn
        in GS.Monster
          { GS.monsterId = monsterId'
          , GS.mPosition = toWorldPosition pixelPos
          , GS.mStats = GS.Stats
              { GS.hp = maximumHp
              , GS.maxHp = maximumHp
              , GS.damage = Map.spawnDamage spawn
              , GS.moveSpeed = 68 / worldScale
              , GS.atkSpeed = Map.spawnAttackRate spawn
              , GS.atkRange = 3.0
              }
          , GS.kind = monsterKind
          , GS.mCooldown = 0
          }
      monsters = map makeMonster spawns
      localMap = GS.MapState
        { GS.mapIndex = 1
        , GS.isBossMap = False
        , GS.players = [player]
        , GS.monsters = monsters
        , GS.bullets = []
        , GS.drops = []
        , GS.portal = GS.Portal False (0, 0)
        }
      cooldowns =
        [ (GS.monsterId monster, 0.6 + fromIntegral (GS.monsterId monster `mod` 4) * 0.15)
        | monster <- monsters
        ]
  in state
    { appView = InGameView
    , localMapState = localMap
    , localAttackCooldown = 0
    , localMonsterAttackCooldowns = cooldowns
    , playerPosition = playerStart
    , playerPos = playerStart
    , clearedRooms = []
    , activatedRooms = []
    , currentRoom = Just Map.StartRoom
    , bossDefeated = False
    , playerHealth = fromIntegral maxHealth

    , enemies = map toEnemyData monsters
    , enemyCorpses = []
    , playerAnim = startAnim Idle
    , bossAnim = startAnim Idle
    , bossHealth = 600
    , bossMaxHealth = 600
    , projectiles = []
    , attackEffectTimer = 0
    , tutorialVisible = False
    , statusText = "Diệt hết quái ở phòng trước để mở cửa"
    , isVictory = False
    }

localRunFinished :: ClientState -> Bool
localRunFinished state =
  case GS.players (localMapState state) of
    [] -> True
    player : _ ->
      not (Combat.isPlayerAlive player)
        || bossDefeated state

toEnemyData :: GS.Monster -> EnemyData
toEnemyData monster = EnemyData
  { enemyKind = case GS.kind monster of
      GS.Boss -> HighDamageEnemy
      GS.Regular GS.Melee GS.TankBody -> TankEnemy
      GS.Regular GS.Melee _ -> MeleeEnemy
      GS.Regular GS.Ranged GS.TankBody -> TankEnemy
      GS.Regular GS.Ranged _ -> RangedEnemy
  , enemyPos = toScreenPosition (GS.mPosition monster)
  , enemyCurrentHp = fromIntegral (GS.hp (GS.mStats monster))
  , enemyMaxHp = fromIntegral (GS.maxHp (GS.mStats monster))
  , enemyIsAlive = Combat.isMonsterAlive monster
  , enemyId = GS.monsterId monster
  , enemyAnim = startAnim Idle
  , enemyFacingRight = False
  , enemySprite = monsterSpriteSet (GS.kind monster)
  }

-- Theo MonsterKind của server: Regular AttackType BodyType | Boss
-- Melee = orc (cận chiến), Ranged = mon (bắn đạn), Boss = demon
monsterSpriteSet :: GS.MonsterKind -> SpriteSet
monsterSpriteSet (GS.Regular GS.Melee _)  = OrcSet
monsterSpriteSet (GS.Regular GS.Ranged _) = MonsterSet
monsterSpriteSet GS.Boss                  = DemonSet

purchaseItem :: ShopItemKind -> ClientState -> ClientState
purchaseItem item state
  | goldCoins state < price = showNotice "Not enough gold." ShopView state
  | otherwise =
      let updated = addInventory item (goldCoins state - price) state
      in case item of
        DamageCharm -> updated { playerDamage = playerDamage state + 3 }
        SwiftBoots -> updated { playerMoveSpeed = playerMoveSpeed state + 20 }
        _ -> updated
  where
    (_, _, price, _) = Shop.shopItemDetails item

showNotice :: String -> ViewState -> ClientState -> ClientState
showNotice message previous state = state
  { appView = ShopNoticeView
  , noticeReturnView = previous
  , noticeText = message
  }

addInventory :: ShopItemKind -> Int -> ClientState -> ClientState
addInventory item newGold state = state
  { goldCoins = newGold
  , shopInventory = addCount item (shopInventory state)
  }
  where
    addCount kind [] = [(kind, 1)]
    addCount kind ((existing, count) : rest)
      | kind == existing = (existing, count + 1) : rest
      | otherwise = (existing, count) : addCount kind rest

purchaseUpgrade :: UpgradeKind -> ClientState -> ClientState
purchaseUpgrade upgrade state
  | goldCoins state < price = showNotice "Not enough gold." UpgradeView state
  | otherwise = applyUpgrade upgrade (goldCoins state - price) state
  where
    level = case upgrade of
      MaxHealthUpgrade -> healthUpgradeLevel state
      DamageUpgrade -> damageUpgradeLevel state
      MoveSpeedUpgrade -> speedUpgradeLevel state
    price = Upgrade.upgradePrice level

applyUpgrade :: UpgradeKind -> Int -> ClientState -> ClientState
applyUpgrade upgrade newGold state = case upgrade of
  MaxHealthUpgrade ->
    let updated = setPlayerHealth (playerHealth state + 20) state
        updatedMap = localMapState updated
        maxHealth = round (playerMaxHealth state + 20)
    in updated
      { goldCoins = newGold
      , healthUpgradeLevel = healthUpgradeLevel state + 1
      , playerMaxHealth = playerMaxHealth state + 20
      , localMapState = updatedMap
          { GS.players = map
              (\player -> player
                { GS.stats = (GS.stats player) { GS.maxHp = maxHealth } })
              (GS.players updatedMap)
          }
      }
  DamageUpgrade -> state
    { goldCoins = newGold
    , damageUpgradeLevel = damageUpgradeLevel state + 1
    , playerDamage = playerDamage state + 5
    }
  MoveSpeedUpgrade -> state
    { goldCoins = newGold
    , speedUpgradeLevel = speedUpgradeLevel state + 1
    , playerMoveSpeed = playerMoveSpeed state + 10
    }

useInventoryItem :: Int -> ClientState -> ClientState
useInventoryItem slot state = case drop slot consumables of
  (item : _) | inventoryCount item (shopInventory state) > 0 ->
    consumeItem item state
  _ -> state
  where
    consumables =
      [ SmallHealthPotion, LargeHealthPotion ]

inventoryCount :: ShopItemKind -> [(ShopItemKind, Int)] -> Int
inventoryCount item inventory =
  sum [count | (kind, count) <- inventory, kind == item]

consumeItem :: ShopItemKind -> ClientState -> ClientState
consumeItem item state =
  let hpGain = case item of
        SmallHealthPotion -> 25
        LargeHealthPotion -> 60
        _ -> 0
      remaining = removeOne item (shopInventory state)
      newHealth = min (playerMaxHealth state) (playerHealth state + hpGain)
  in (setPlayerHealth newHealth state)
    { playerHealth = newHealth
    , shopInventory = remaining
    }

setPlayerHealth :: Float -> ClientState -> ClientState
setPlayerHealth health state = state
  { playerHealth = health
  , localMapState = mapState
      { GS.players = case GS.players mapState of
          [] -> []
          player : rest ->
            player
              { GS.stats = (GS.stats player) { GS.hp = max 0 (round health) } }
              : rest
      }
  }
  where
    mapState = localMapState state

removeOne :: ShopItemKind -> [(ShopItemKind, Int)] -> [(ShopItemKind, Int)]
removeOne _ [] = []
removeOne item ((kind, count) : rest)
  | item == kind && count > 1 = (kind, count - 1) : rest
  | item == kind = rest
  | otherwise = (kind, count) : removeOne item rest

interactWithChest :: ClientState -> ClientState
interactWithChest state
  | chestState (treasureChest state) == ChestClosed
      && closeEnough (playerPosition state) (chestPosition (treasureChest state)) =
          state
            { appView = ChestRewardView
            , returnView = InGameView
            , treasureChest = (treasureChest state) { chestState = ChestClaimed }
            }
  | otherwise = state

closeEnough :: (Float, Float) -> (Float, Float) -> Bool
closeEnough (x1, y1) (x2, y2) =
  sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)) <= 70

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (value : rest) = value : dropLast rest
