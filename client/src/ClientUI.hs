module ClientUI
  ( UiAction(..)
  , UiResult(..)
  , reduceUi
  , startLocalRun
  , toEnemyData
  , chestRewardFor
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
import qualified Logic.MapGen
import qualified StageLayout as Stage
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
  VictoryView -> Just (playAgain state)
  DefeatView -> Just (playAgain state)
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
      Just (playAgain state)
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
        , onlinePlayerId = Nothing
        , statusText = "Press JOIN (blank code = create a new room)."
        }
  | otherwise = Nothing

clickLobby :: (Float, Float) -> ClientState -> Maybe ClientState
clickLobby position state
  | insideRect position (-405, 143) 170 32 =
      Just state { statusText = "Room-code copy is unavailable in this window." }
  | insideRect position (0, 165) 320 34 =
      Just state { nameInputActive = True, roomInputActive = False
                 , statusText = "Type your name (letters/digits), then press Enter." }
  | insideRect position (0, 215) 320 58 =
      Just state { roomInputActive = True, nameInputActive = False
                 , statusText = "Enter letters, digits or -." }
  | insideRect position (235, 215) 130 58 =
      Just (submitRoomCode (state { roomInputActive = False }))
  | insideRect position (-450, -270) 200 52 =
      Just state { appView = MapSelectView, isMultiplayer = False, onlinePlayerId = Nothing, roomCode = "LOCAL-01" }
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
        (min (playerMaxHealth state) (playerHealth state + rewardHealth reward))
        state
          { appView = if offlineBoss then VictoryView else returnView state
          , isVictory = offlineBoss || isVictory state
          , goldCoins = goldCoins state + rewardGold reward
          , gems = gems state + rewardGems reward
          }
  | otherwise = Nothing
  where
    reward = chestRewardFor state
    -- offline: mở rương boss xong mới thắng ván (online: ván kết thúc khi vào cổng)
    offlineBoss = onlinePlayerId state == Nothing && bossChestActive state

-- Rương ở màn boss (sau khi boss chết) thưởng nhiều hơn rương thường.
bossChestActive :: ClientState -> Bool
bossChestActive st = case onlinePlayerId st of
  Just _  -> GS.mapIndex (localMapState st) == Stage.lastStage
  Nothing -> bossDefeated st

chestRewardFor :: ClientState -> ChestReward
chestRewardFor st
  | bossChestActive st = ChestReward 300 60 10
  | otherwise = ChestReward 120 25 2

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
      Just state { appView = MainMenuView, isMultiplayer = False, onlinePlayerId = Nothing, roomCode = "LOCAL-01" }
  | otherwise = Nothing

clickResult :: (Float, Float) -> ClientState -> Maybe ClientState
clickResult position state
  | insideRect position (-145, -130) 250 58 =
      Just (playAgain state)
  | insideRect position (155, -130) 210 58 =
      -- online: về menu thì rời phòng (Main.handleInput gửi LeaveRoom khi onlinePlayerId biến mất)
      Just state { appView = MainMenuView, isMultiplayer = False, onlinePlayerId = Nothing, roomCode = "LOCAL-01" }
  | otherwise = Nothing

-- Online: chơi tiếp = về lobby của phòng đang ở. Offline: chạy ván mới.
playAgain :: ClientState -> ClientState
playAgain state
  | isMultiplayer state =
      state { appView = LobbyView, isReady = False, statusText = "Back in the lobby." }
  | otherwise = startLocalRun state

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
  let role = if selectedChar state == Archer then GS.Archer else GS.Warrior
      meta = GS.Meta (gems state) (healthUpgradeLevel state) (damageUpgradeLevel state) (speedUpgradeLevel state)
      profile = GS.PlayerProfile "Player" role (playerLevel state) (playerExperience state)
                  (goldCoins state) False meta
      -- offline dùng đúng bản đồ + luật của chế độ online (phòng 1: cửa mở, phòng 2: quái, phòng 3: boss)
      localMap = Logic.MapGen.newMapState [(1, profile)]
      (playerStats, playerStart) = case GS.players localMap of
        (p : _) -> (GS.stats p, toScreenPosition (GS.position p))
        [] -> (GS.Stats 150 150 25 4.5 0.45 3.0, (0, 0))
  in state
    { appView = InGameView
    , sharedState = GS.initialGameState
    , localMapState = localMap
    , localAttackCooldown = 0
    , localMonsterAttackCooldowns = []
    , playerPosition = playerStart
    , playerPos = playerStart
    , clearedRooms = []
    , activatedRooms = []
    , currentRoom = Just Map.StartRoom
    , bossDefeated = False
    , playerHealth = fromIntegral (GS.hp playerStats)
    , playerMaxHealth = fromIntegral (GS.maxHp playerStats)
    , playerDamage = fromIntegral (GS.damage playerStats)
    , playerMoveSpeed = GS.moveSpeed playerStats * worldScale
    , enemies = []
    , enemyCorpses = []
    , playerAnim = startAnim Idle
    , bossAnim = startAnim Idle
    , bossHealth = 600
    , bossMaxHealth = 600
    , projectiles = []
    , attackEffectTimer = 0
    , tutorialVisible = False
    , statusText = "Clear the room, then step into the golden zone"
    , damageTexts = []
    , doorOpen = 1
    , stageClock = 0
    , treasureChest = ChestData (0, 0) ChestClaimed
    , onlinePlayerId = Just 1    -- cùng đường vẽ / luật với online; offline chỉ là không có mạng (isMultiplayer = False)
    , isMultiplayer = False
    , allies = []
    , isVictory = False
    }

localRunFinished :: ClientState -> Bool
localRunFinished state =
  case GS.players (localMapState state) of
    [] -> True
    player : _ -> not (Combat.isPlayerAlive player)

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
          hurt = playerHealth state < playerMaxHealth state
      in case item of
        DamageCharm -> syncPlayerDamage updated { playerDamage = playerDamage state + 3 }
        SwiftBoots -> syncPlayerSpeed updated { playerMoveSpeed = playerMoveSpeed state + 20 }
        -- bình máu: đang bị thương thì dùng ngay khi mua, còn đầy máu thì cất vào túi
        SmallHealthPotion | hurt -> consumeItem item updated
        LargeHealthPotion | hurt -> consumeItem item updated
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
  DamageUpgrade -> syncPlayerDamage state
    { goldCoins = newGold
    , damageUpgradeLevel = damageUpgradeLevel state + 1
    , playerDamage = playerDamage state + 5
    }
  MoveSpeedUpgrade -> syncPlayerSpeed state
    { goldCoins = newGold
    , speedUpgradeLevel = speedUpgradeLevel state + 1
    , playerMoveSpeed = playerMoveSpeed state + 10
    }

-- Đưa sát thương mới vào nhân vật đang chơi (offline đánh theo GS.damage).
syncPlayerDamage :: ClientState -> ClientState
syncPlayerDamage state = state
  { localMapState = mapState
      { GS.players = map
          (\p -> p { GS.stats = (GS.stats p) { GS.damage = max 0 (round (playerDamage state)) } })
          (GS.players mapState)
      }
  }
  where mapState = localMapState state

-- Đưa tốc chạy mới vào nhân vật đang chơi (đơn vị thế giới = px / 35).
syncPlayerSpeed :: ClientState -> ClientState
syncPlayerSpeed state = state
  { localMapState = mapState
      { GS.players = map
          (\p -> p { GS.stats = (GS.stats p) { GS.moveSpeed = playerMoveSpeed state / worldScale } })
          (GS.players mapState)
      }
  }
  where mapState = localMapState state

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
  sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)) <= 110

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (value : rest) = value : dropLast rest
