module Main where

import Control.Concurrent.MVar (tryReadMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.IO.Game
  ( Event(..)
  , Key(..)
  , KeyState(..)
  , MouseButton(..)
  , SpecialKey(..)
  , playIO
  )
import Animation (AnimKind(..), AnimState(..), SpriteSet(..), animFinished, setAnimKind, startAnim, stepAnim)
import Sprites (framesOf, playerSet)
import Assets (loadAssets)
import ClientUI (UiAction(..), UiResult(..), reduceUi, startLocalRun, toEnemyData, stateMeta, loadMetaInto)
import Components.UI (insideRect)
import qualified MapLayout as Map
import qualified StageLayout as Stage
import Network
  ( NetworkConnection(..)
  , connectToServer
  )
import Protocol (ClientMsg(StartGame, InputMsg, LeaveRoom, AdjustGold, HealSelf, BoostStats, SetReady, ChooseRole, SaveMeta))
import qualified Types.GameState as GS
import qualified Data.Map as M
import Logic.Control (PlayerInput(..), applyInputs)
import Logic.GameLoop (updateStep, generateDropsIO)
import Logic.Portal (roomOutcome)
import Input
  ( InputRefs
  , handleEvent
  , movementDirection
  , newInputRefs
  , isAttackHeld
  , takeAttackRequest
  )
import Render (renderClient)
import Types

-- Kết nối mạng hiện tại (Nothing = đang chơi offline) và địa chỉ server.
data Net = Net
  { netConn :: IORef (Maybe NetworkConnection)
  , netHost :: String
  , netPort :: Int
  , netSent :: IORef (Maybe ((Float, Float), Bool))   -- (hướng đi, đang đánh) đã gửi lần cuối
  }

main :: IO ()
main = runClient

-- Địa chỉ server: đọc dòng đầu của file server.txt (dạng host:port),
-- nếu không có thì dùng biến môi trường SK_SERVER, cuối cùng là 127.0.0.1:8080.
loadServerAddress :: IO (String, Int)
loadServerAddress = do
  fileResult <- try (readFile "server.txt") :: IO (Either SomeException String)
  envResult <- lookupEnv "SK_SERVER"
  let fromFile = case fileResult of
        Right txt | (l : _) <- lines txt, not (null (trim l)) -> Just (trim l)
        _ -> Nothing
      raw = fromMaybe "127.0.0.1:8080" (maybe envResult Just fromFile)
      (host, rest) = break (== ':') raw
      port = fromMaybe 8080 (readMaybe (drop 1 rest))
  pure (host, port)
  where
    trim = filter (`notElem` " \r\n\t")

runClient :: IO ()
runClient = do
  assets <- loadAssets
  inputRefs <- newInputRefs
  (host, port) <- loadServerAddress
  connRef <- newIORef Nothing
  sentRef <- newIORef Nothing
  let net = Net connRef host port sentRef
      window = InWindow "Soul Knight Haskell" (1280, 720) (100, 100)
      initialState = initialClientState assets
  playIO window black 60 initialState
    (pure . renderClient)
    (handleInput net inputRefs)
    (updateFrame net inputRefs)

handleInput :: Net -> InputRefs -> Event -> ClientState -> IO ClientState
handleInput net inputRefs event state = do
  next <- handleInputRaw net inputRefs event state
  -- thoát khỏi phòng online (về menu...) thì báo server để phòng không bị kẹt
  when (isMultiplayer state && not (isMultiplayer next)) (leaveRoom net)
  syncEconomy net state next

-- Chơi online: vàng và máu do server giữ. Rương / shop / bình thuốc đổi vàng, máu
-- ở máy mình thì phải báo lên server, nếu không số liệu server sẽ ghi đè ngay.
syncEconomy :: Net -> ClientState -> ClientState -> IO ClientState
syncEconomy net before after = do
  current <- readIORef (netConn net)
  case current of
    Just conn
      | onlinePlayerId after /= Nothing -> do
          let dGold = goldCoins after - goldCoins before
              dMax = round (playerMaxHealth after - playerMaxHealth before) :: Int
              dHp = round (playerHealth after - playerHealth before) :: Int
              dDmg = round (playerDamage after - playerDamage before) :: Int
              dSpd = (playerMoveSpeed after - playerMoveSpeed before) / worldScale
          when (dGold /= 0) $ sendClientMessage conn (AdjustGold dGold)
          -- gem và cấp nâng cấp được lưu vào database
          when (metaOf after /= metaOf before) $ sendClientMessage conn (SaveMeta (metaOf after))
          -- tăng chỉ số trước (kèm máu tối đa), phần máu hồi thêm gửi sau
          when (dDmg > 0 || dSpd > 0 || dMax > 0) $
            sendClientMessage conn (BoostStats (max 0 dDmg) (max 0 dSpd) (max 0 dMax))
          let heal = dHp - max 0 dMax
          when (heal > 0) $ sendClientMessage conn (HealSelf heal)
          pure after
    _ -> pure after

metaOf :: ClientState -> GS.Meta
metaOf = stateMeta

handleInputRaw :: Net -> InputRefs -> Event -> ClientState -> IO ClientState
handleInputRaw net inputRefs event state = do
  handleEvent (const (pure ())) inputRefs event
  let positionedState = case event of
        EventMotion position -> state { mousePos = position }
        EventKey (MouseButton _) _ _ position -> state { mousePos = position }
        _ -> state
  case event of
    EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      applyUiAction inputRefs UiEnter positionedState
    EventKey (Char key) Down _ _ ->
      applyUiAction inputRefs (UiKey key) positionedState
    EventKey (MouseButton LeftButton) Down _ (x, y)
      | appView state == LobbyView -> do
          handled <- lobbyClick net (x, y) positionedState
          case handled of
            Just nextState -> pure nextState
            Nothing -> applyUiAction inputRefs (UiClick (x, y)) positionedState
      | otherwise -> applyUiAction inputRefs (UiClick (x, y)) positionedState
    _ -> pure positionedState

-- Các nút ở lobby cần gọi mạng. Trả Nothing nếu để UI offline cũ xử lý.
lobbyClick :: Net -> (Float, Float) -> ClientState -> IO (Maybe ClientState)
lobbyClick net position state
  -- JOIN: nối server; ô mã phòng trống = tạo phòng mới
  | insideRect position (235, 215) 130 58 = do
      current <- readIORef (netConn net)
      case current of
        Just _ -> pure (Just state { statusText = "Already connected to a room." })
        Nothing -> do
          let code = map toUpper (roomInput state)
          result <- connectToServer (netHost net) (netPort net) code (accountName state)
          case result of
            Left err -> pure (Just (errorPopup LobbyView (friendlyError err) state))
            Right conn -> do
              let savedProfile = lookup (myPlayerId conn) (GS.profiles (initialGameState conn))
              writeIORef (netConn net) (Just conn)
              writeIORef (netSent net) Nothing
              pure (Just (maybe id (loadMetaInto . GS.profileMeta) savedProfile state
                { roomCode = joinedRoomCode conn
                , roomInput = ""
                , roomInputActive = False
                , nameInputActive = False
                , isMultiplayer = True
                , onlinePlayerId = Just (myPlayerId conn)
                , sharedState = initialGameState conn
                , statusText = "Joined room " ++ joinedRoomCode conn }))
  -- Nút giữa: chủ phòng = START RUN (khi mọi người đã sẵn sàng), người khác = READY UP
  | insideRect position (0, -270) 260 52 = do
      current <- readIORef (netConn net)
      case current of
        Nothing -> pure Nothing
        Just conn -> do
          let profs = GS.profiles (sharedState state)
              ids = map fst profs
              isHost = ids /= [] && myPlayerId conn == minimum ids
              others = [p | (i, p) <- profs, i /= myPlayerId conn]
              mineReady = or [GS.profileReady p | (i, p) <- profs, i == myPlayerId conn]
          if isHost
            then if all GS.profileReady others
              then do
                sendClientMessage conn StartGame
                pure (Just state { statusText = "Starting..." })
              else pure (Just state { statusText = "Waiting for every player to press READY UP." })
            else do
              sendClientMessage conn (SetReady (not mineReady))
              pure (Just state { statusText = if mineReady then "You are not ready." else "You are ready." })
  -- BACK: rời phòng rồi để UI cũ quay lại màn trước
  | insideRect position (-450, -270) 200 52 = do
      leaveRoom net
      pure Nothing
  | otherwise = pure Nothing

leaveRoom :: Net -> IO ()
leaveRoom net = do
  current <- readIORef (netConn net)
  case current of
    Just conn -> sendClientMessage conn LeaveRoom
    Nothing -> pure ()
  writeIORef (netConn net) Nothing
  writeIORef (netSent net) Nothing

applyUiAction :: InputRefs -> UiAction -> ClientState -> IO ClientState
applyUiAction inputRefs action state =
  case reduceUi action state of
    UiQuit -> exitSuccess
    UiHandled nextState -> do
      case action of
        UiClick _ -> do
          _ <- takeAttackRequest inputRefs
          pure ()
        _ -> pure ()
      pure nextState
    UiUnhandled -> pure state

updateFrame :: Net -> InputRefs -> Float -> ClientState -> IO ClientState
updateFrame net inputRefs deltaTime state = do
  current <- readIORef (netConn net)
  case current of
    Nothing -> updateLocal inputRefs deltaTime state
    Just conn -> updateOnline net conn inputRefs deltaTime state

-- Offline: chạy đúng luật + bản đồ của server ngay trong máy (cùng hàm applyInputs / updateStep),
-- rồi đổ kết quả vào ClientState như khi nhận trạng thái từ server.
updateLocal :: InputRefs -> Float -> ClientState -> IO ClientState
updateLocal inputRefs deltaTime state
  | appView state /= InGameView = do
      _ <- takeAttackRequest inputRefs
      pure state
  | otherwise = do
      direction <- movementDirection inputRefs
      held <- isAttackHeld inputRefs
      _ <- takeAttackRequest inputRefs
      let dt = max 0 (min 0.05 deltaTime)
          ms0 = localMapState state
          inputs = M.singleton 1 (PlayerInput direction held)
          (ms1, cds) = applyInputs dt inputs (M.fromList (localMonsterAttackCooldowns state)) ms0
          ms2 = updateStep dt [] ms1
          aliveIds = map GS.monsterId (GS.monsters ms2)
          killed = [m | m <- GS.monsters ms1, GS.monsterId m `notElem` aliveIds]
      newDrops <- concat <$> mapM generateDropsIO killed
      let ms3 = ms2 { GS.drops = GS.drops ms2 ++ newDrops }
          (ms4, gold', lv', xp') = collectEconomy state ms3
          before = state
            { goldCoins = gold'
            , playerLevel = lv'
            , playerExperience = xp'
            , localMonsterAttackCooldowns = M.toList cds
            }
          stepped = applyServerMap dt direction (sharedState state) ms4 before
      pure $ case roomOutcome ms4 of
        Just GS.Victory -> stepped { appView = VictoryView, isVictory = True }
        Just GS.Defeat -> stepped { appView = DefeatView }
        Nothing -> stepped

-- Vàng / EXP nhặt được chuyển vào ClientState (server online làm việc này qua pending + hồ sơ);
-- lên cấp thì hồi đầy máu.
collectEconomy :: ClientState -> GS.MapState -> (GS.MapState, Int, Int, Int)
collectEconomy st ms = case GS.players ms of
  (p : rest) ->
    let gold' = max 0 (goldCoins st + GS.pendingGold p)
        (lv', xp') = levelFor (playerLevel st) (playerExperience st + GS.pendingExp p)
        stats = GS.stats p
        stats' = if lv' > playerLevel st && GS.status p == GS.Alive
                   then stats { GS.hp = GS.maxHp stats } else stats
        p' = p { GS.pendingGold = 0, GS.pendingExp = 0, GS.stats = stats' }
    in (ms { GS.players = p' : rest }, gold', lv', xp')
  [] -> (ms, goldCoins st, playerLevel st, playerExperience st)

-- Chơi online: gửi input lên server, nhận GameState mới nhất và đổ vào
-- ClientState để Render cũ vẽ lại (không tự mô phỏng gameplay ở client).
updateOnline :: Net -> NetworkConnection -> InputRefs -> Float -> ClientState -> IO ClientState
updateOnline net conn inputRefs dt state = do
  failure <- tryReadMVar (connectionFailure conn)
  case failure of
    Just err -> do
      writeIORef (netConn net) Nothing
      pure state
        { appView = ShopNoticeView
        , returnView = LobbyView
        , noticeReturnView = LobbyView
        , noticeText = friendlyError err
        , isMultiplayer = False
        , onlinePlayerId = Nothing
        , statusText = "Disconnected."
        }
    Nothing -> do
      latest <- tryReadMVar (latestGameState conn)
      let gs = fromMaybe (sharedState state) latest
      direction <- movementDirection inputRefs
      _ <- takeAttackRequest inputRefs
      case GS.phase gs of
        GS.Lobby -> do
          -- báo nghề đang chọn lên server (server đổi hình / chỉ số / tầm đánh theo nghề)
          let wanted = if selectedChar state == Archer then GS.Archer else GS.Warrior
              mine = [GS.profileRole p | (i, p) <- GS.profiles gs, Just i == onlinePlayerId state]
          when (any (/= wanted) mine) $ sendClientMessage conn (ChooseRole wanted)
          pure state
            { sharedState = gs
            , statusText = "Players in room: " ++ show (length (GS.profiles gs)) ++ "/4"
            }
        GS.Playing ms -> do
          -- tự động đánh (server chọn mục tiêu gần nhất trong tầm)
          let move = if appView state == InGameView then direction else (0, 0)
          -- chỉ đánh khi giữ chuột trái (và đang ở màn chơi, không phải shop / pause)
          held <- isAttackHeld inputRefs
          let attacking = held && appView state == InGameView
              sentKey = (move, attacking)
          -- chỉ gửi khi hướng đi / trạng thái đánh đổi (server giữ input cuối cùng)
          lastKey <- readIORef (netSent net)
          when (lastKey /= Just sentKey) $ do
            sendClientMessage conn (InputMsg move (0, 0) attacking)
            writeIORef (netSent net) (Just sentKey)
          let base = if appView state == LobbyView then freshOnlineRun state else state
          let next = applyServerMap dt direction gs ms base
          -- lên cấp thì hồi đầy máu (server giữ máu nên báo lên)
          when (playerLevel next > playerLevel base && appView state /= LobbyView) $
            sendClientMessage conn (HealSelf 1000)
          pure next
        GS.RoomResult result ->
          pure state
            { sharedState = gs
            , appView = if appView state == InGameView
                then (if result == GS.Victory then VictoryView else DefeatView)
                else appView state
            }

-- Đưa giá trị `cur` tiến dần tới `target`, mỗi lần tối đa `amount` (dùng cho hoạt ảnh cửa).
stepToward :: Float -> Float -> Float -> Float
stepToward target amount cur
  | cur < target = min target (cur + amount)
  | otherwise = max target (cur - amount)

-- Cùng công thức với server (cấp n cần n*100 EXP).
levelFor :: Int -> Int -> (Int, Int)
levelFor lv xp
  | xp >= lv * 100 = levelFor (lv + 1) (xp - lv * 100)
  | otherwise = (lv, xp)

-- Tên gửi lên server; để trống thì dùng "Player".
accountName :: ClientState -> String
accountName st = case filter (/= ' ') (playerName st) of
  [] -> "Player"
  n  -> n

-- Khởi tạo các trường phụ (HUD, animation...) bằng startLocalRun rồi xoá quái giả.
freshOnlineRun :: ClientState -> ClientState
freshOnlineRun state =
  let fresh = startLocalRun state
  in fresh
    { localMapState = (localMapState fresh) { GS.monsters = [] }
    , enemies = []
    , enemyCorpses = []
    , allies = []
    , damageTexts = []
    , treasureChest = ChestData (0, 0) ChestClaimed
    , isMultiplayer = True
    , onlinePlayerId = onlinePlayerId state
    }

-- Đổ MapState từ server vào ClientState.
applyServerMap :: Float -> (Float, Float) -> GS.GameState -> GS.MapState -> ClientState -> ClientState
applyServerMap dt (dx, _) gs ms st = st
  { sharedState = gs
  , localMapState = ms
  , playerPosition = shown
  , playerPos = shown
  , enemies = newEnemies
  , allies = newAllies
  , enemyCorpses = if sameStage
      then map (\e -> e { enemyAnim = stepAnim dt (enemyAnim e) }) (enemyCorpses st ++ newCorpses)
      else []   -- sang phòng mới: bỏ xác quái phòng cũ
  , clearedRooms = cleared
  , currentRoom = Map.roomAt (selectedMapSize st) shown
  , bossDefeated = not (any ((== 900) . GS.monsterId) (GS.monsters ms))
  , doorOpen = stepToward (if null (GS.monsters ms) then 1 else 0) (dt * 2.5) (doorOpen st)
  , stageClock = stageClock st + dt
  , playerLevel = shownLevel
  , playerExperience = shownExp
  , goldCoins = shownGold
  , playerHealth = maybe (playerHealth st) (fromIntegral . GS.hp . GS.stats) me
  , playerMaxHealth = maybe (playerMaxHealth st) (fromIntegral . max 1 . GS.maxHp . GS.stats) me
  , playerAnim = stepAnim dt (pickAnim (playerSet (selectedChar st)) (any nearestIsMe hitMonsters) moved (playerAnim st))
  , damageTexts = stepDamageTexts dt (damageEvents (localMapState st) ms) (damageTexts st)
  , treasureChest = chestNow
  , attackEffectTimer = if null hitMonsters then max 0 (attackEffectTimer st - dt) else 0.18
  , attackEffectPos = case hitMonsters of
      (m : _) -> Map.toScreenPosition (GS.mPosition m)
      [] -> attackEffectPos st
  , playerFacingRight = if dx > 0 then True else if dx < 0 then False else playerFacingRight st
  }
  where
    me = find ((== onlinePlayerId st) . Just . GS.playerId) (GS.players ms)
    -- Rương online: hiện ở chỗ boss chết, hoặc ở chỗ quái cuối cùng chết khi dọn sạch phòng quái.
    -- Sang màn khác thì rương cũ biến mất.
    chestNow = case GS.monsters (localMapState st) of
      prev
        | not sameStage -> ChestData (0, 0) ChestClaimed
        | (b : _) <- [m | m <- prev, GS.monsterId m == 900], GS.monsterId b `notElem` newIds ->
            ChestData (Map.toScreenPosition (GS.mPosition b)) ChestClosed
        | (m : _) <- prev, null (GS.monsters ms), GS.mapIndex ms < Stage.lastStage ->
            ChestData (Map.toScreenPosition (GS.mPosition m)) ChestClosed
        | otherwise -> treasureChest st
    -- vàng / EXP / cấp: hồ sơ đã lưu + phần nhặt được trong ván này (server tính)
    myProfile = onlinePlayerId st >>= \i -> lookup i (GS.profiles gs)
    (shownLevel, shownExp) = case myProfile of
      Just pr -> levelFor (GS.profileLv pr) (GS.profileExp pr + maybe 0 GS.pendingExp me)
      Nothing -> (playerLevel st, playerExperience st)
    shownGold = case myProfile of
      Just pr -> GS.profileGold pr + maybe 0 GS.pendingGold me
      Nothing -> goldCoins st
    myScreen = maybe (playerPosition st) (Map.toScreenPosition . GS.position) me
    -- server gửi 30 lần/giây còn màn hình vẽ 60 khung/giây: làm mượt vị trí để khỏi giật
    alpha = smoothFactor dt
    shown = smoothPos alpha (playerPosition st) myScreen
    moved = distance2 shown (playerPosition st) > 0.04
    distance2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)
    cleared =
      [ r
      | r <- [NormalA, NormalB, BossRoom]
      , not (any ((== Just r) . Map.homeRoomOfMonster . GS.monsterId) (GS.monsters ms))
      ]
    oldById = [(enemyId e, e) | e <- enemies st]
    newIds = map GS.monsterId (GS.monsters ms)
    -- Nhận ra đòn đánh từ dữ liệu server (client online không tự mô phỏng trận đánh):
    --  * quái mất máu hoặc biến mất  = vừa bị đánh trúng -> hiệu ứng trúng đòn
    --  * hồi chiêu của quái tăng lên = quái vừa ra đòn   -> hoạt ảnh Attack
    prevMonsters = GS.monsters (localMapState st)
    prevHp = [(GS.monsterId m, GS.hp (GS.mStats m)) | m <- prevMonsters]
    prevCooldown = [(GS.monsterId m, GS.mCooldown m) | m <- prevMonsters]
    nowIds = map GS.monsterId (GS.monsters ms)
    sameStage = GS.mapIndex (localMapState st) == GS.mapIndex ms
    hitMonsters = if not sameStage then [] else
      [ m | m <- GS.monsters ms
          , Just h0 <- [lookup (GS.monsterId m) prevHp]
          , GS.hp (GS.mStats m) < h0 ]
      ++ [ m | m <- prevMonsters, GS.monsterId m `notElem` nowIds ]
    attackedIds =
      [ GS.monsterId m
      | m <- GS.monsters ms
      , Just c0 <- [lookup (GS.monsterId m) prevCooldown]
      , GS.mCooldown m > c0 + 0.001 ]
    -- đòn đánh trúng quái này là của mình nếu mình là người chơi sống gần quái nhất
    nearestIsMe m =
      case [p | p <- GS.players ms, GS.status p == GS.Alive] of
        [] -> False
        ps -> let best = foldr1 (\a b -> if distance2 (GS.mPosition m) (GS.position a)
                                             <= distance2 (GS.mPosition m) (GS.position b)
                                          then a else b) ps
              in Just (GS.playerId best) == onlinePlayerId st
    -- Đòn đang chạy dở thì giữ nguyên tới khi hết, sau đó đi / đứng yên.
    pickAnim set attacked isMoving old
      | attacked = startAnim Attack
      | animKind old == Attack && not (animFinished (framesOf (appAssets st) set Attack) old) = old
      | isMoving = setAnimKind Walk old
      | otherwise = setAnimKind Idle old
    newEnemies = map carry (map toEnemyData (GS.monsters ms))
    carry e = case lookup (enemyId e) oldById of
      Nothing -> e
      Just old ->
        let shownE = smoothPos alpha (enemyPos old) (enemyPos e)
            (ox, _) = enemyPos old
            (nx, _) = shownE
            movedE = distance2 (enemyPos old) shownE > 0.04
        in e { enemyPos = shownE
             , enemyAnim = stepAnim dt (pickAnim (enemySprite old) (enemyId e `elem` attackedIds) movedE (enemyAnim old))
             , enemyFacingRight = if nx > ox then True else if nx < ox then False else enemyFacingRight old }
    -- đồng đội: mọi người chơi còn sống trừ mình, có sprite và vị trí làm mượt
    oldAllies = [(allyId x, x) | x <- allies st]
    newAllies =
      [ mkAlly p
      | p <- GS.players ms
      , Just (GS.playerId p) /= onlinePlayerId st
      , GS.status p == GS.Alive
      ]
    mkAlly p =
      let pid = GS.playerId p
          target = Map.toScreenPosition (GS.position p)
          sprite = case lookup pid (GS.profiles gs) of
            Just prof | GS.profileRole prof == GS.Archer -> ArcherSet
            _ -> SoldierSet
      in case lookup pid oldAllies of
           Nothing -> AllyData pid ("P" ++ show pid) target (startAnim Idle) True sprite
           Just old ->
             let shownA = smoothPos alpha (allyPos old) target
                 (ox, _) = allyPos old
                 (nx, _) = shownA
                 movedA = distance2 (allyPos old) shownA > 0.04
             in old
                  { allyPos = shownA
                  , allySprite = sprite
                  , allyAnim = stepAnim dt (setAnimKind (if movedA then Walk else Idle) (allyAnim old))
                  , allyFacingRight = if nx > ox then True else if nx < ox then False else allyFacingRight old
                  }
    -- quái có ở frame trước nhưng không còn trong danh sách sống = vừa chết
    newCorpses =
      [ old { enemyIsAlive = False, enemyAnim = startAnim Die }
      | old <- enemies st
      , enemyId old `notElem` newIds
      ]

-- Hệ số làm mượt theo thời gian khung hình (hằng số 18 ~ trễ khoảng 55 ms).
smoothFactor :: Float -> Float
smoothFactor dt = 1 - exp (negate 18 * max 0 dt)

-- Kéo vị trí đang hiển thị về phía vị trí server; nhảy quá xa (mới vào ván) thì đặt thẳng.
smoothPos :: Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
smoothPos a (x0, y0) (x1, y1)
  | dx * dx + dy * dy > 200 * 200 = (x1, y1)
  | otherwise = (x0 + dx * a, y0 + dy * a)
  where
    dx = x1 - x0
    dy = y1 - y0

-- Lỗi mạng hiện bằng popup (không tràn màn hình): thông điệp ngắn, chi tiết kỹ thuật bỏ đi.
errorPopup :: ViewState -> String -> ClientState -> ClientState
errorPopup back message state = state
  { appView = ShopNoticeView
  , returnView = back
  , noticeReturnView = back
  , noticeText = message
  }

friendlyError :: String -> String
friendlyError err
  | length err <= 45 && not (any (`isInfixOf` err) ["Exception", "exception", "Connection", "refused"]) = err
  | any (`isInfixOf` err) ["refused", "ConnectionFailure", "Connection", "timed out"] =
      "Cannot reach the server. Try again later."
  | otherwise = "Network error. Please try again."
