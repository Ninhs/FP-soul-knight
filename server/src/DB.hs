{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module DB
  ( SavedPlayer(..)
  , RunPlayer(..)
  , initSchema
  , loadPlayer
  , saveRun
  , saveProgress
  , addExp
  , expToNext
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack)
import Data.String (fromString)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple

import Config (getDatabaseUrl)
import Types.GameState

-- Mỗi thao tác mở một kết nối riêng rồi đóng lại (Neon tự ngắt kết nối nhàn rỗi,
-- và thao tác DB chỉ xảy ra lúc vào phòng / hết ván nên không tốn kém).
withConn :: (Connection -> IO a) -> IO a
withConn f = do
  url <- getDatabaseUrl
  bracket (connectPostgreSQL (pack url)) close f

-- Mọi thao tác DB đều "best effort": lỗi chỉ in ra, không làm sập game.
safely :: String -> IO a -> IO (Maybe a)
safely label act = do
  r <- try act
  case r of
    Right a -> pure (Just a)
    Left (e :: SomeException) -> putStrLn ("[DB] " ++ label ++ " loi: " ++ show e) >> pure Nothing

-- Tạo bảng nếu chưa có (khớp schema.sql; chạy an toàn nhiều lần).
initSchema :: IO ()
initSchema = do
  _ <- safely "initSchema" $ withConn $ \conn -> do
    _ <- execute_ conn $ fromString $ unlines
      [ "CREATE TABLE IF NOT EXISTS players ("
      , "  id SERIAL PRIMARY KEY,"
      , "  name TEXT NOT NULL,"
      , "  created_at TIMESTAMP DEFAULT NOW(),"
      , "  role TEXT NOT NULL DEFAULT 'Warrior',"
      , "  level INT NOT NULL DEFAULT 1,"
      , "  exp INT NOT NULL DEFAULT 0,"
      , "  gold INT NOT NULL DEFAULT 500,"
      , "  total_runs INT NOT NULL DEFAULT 0,"
      , "  total_wins INT NOT NULL DEFAULT 0,"
      , "  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW());"
      , "ALTER TABLE players ALTER COLUMN gold SET DEFAULT 500;"
      , "CREATE UNIQUE INDEX IF NOT EXISTS players_name_key ON players ((LOWER(name)));"
      , "CREATE TABLE IF NOT EXISTS game_sessions ("
      , "  id SERIAL PRIMARY KEY,"
      , "  room_code TEXT NOT NULL,"
      , "  host_player_id INT REFERENCES players(id) ON DELETE SET NULL,"
      , "  started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),"
      , "  ended_at TIMESTAMPTZ,"
      , "  result TEXT CHECK (result IN ('Victory','Defeat')),"
      , "  maps_cleared INT NOT NULL DEFAULT 0);"
      , "CREATE TABLE IF NOT EXISTS session_players ("
      , "  session_id INT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,"
      , "  player_id INT NOT NULL REFERENCES players(id) ON DELETE CASCADE,"
      , "  role TEXT NOT NULL,"
      , "  is_host BOOLEAN NOT NULL DEFAULT FALSE,"
      , "  final_status TEXT NOT NULL DEFAULT 'Alive',"
      , "  gold_earned INT NOT NULL DEFAULT 0,"
      , "  exp_earned INT NOT NULL DEFAULT 0,"
      , "  level_before INT NOT NULL DEFAULT 1,"
      , "  level_after INT NOT NULL DEFAULT 1,"
      , "  PRIMARY KEY (session_id, player_id));"
      ]
    putStrLn "[DB] schema san sang"
  pure ()

-- Tiến trình đã lưu của một người chơi.
data SavedPlayer = SavedPlayer
  { spId    :: Int
  , spRole  :: Role
  , spLevel :: Int
  , spExp   :: Int
  , spGold  :: Int
  } deriving (Show)

-- Tìm người chơi theo tên (không phân biệt hoa thường); chưa có thì tạo mới.
-- Nothing nếu không kết nối được DB (khi đó vào phòng với hồ sơ mới, không lưu).
loadPlayer :: String -> IO (Maybe SavedPlayer)
loadPlayer name = do
  r <- safely "loadPlayer" $ withConn $ \conn -> do
    rows <- query conn
      "INSERT INTO players (name) VALUES (?) \
      \ON CONFLICT ((LOWER(name))) DO UPDATE SET updated_at = NOW() \
      \RETURNING id, role, level, exp, gold"
      (Only name) :: IO [(Int, String, Int, Int, Int)]
    pure $ case rows of
      ((i, role, lv, ex, gd) : _) -> Just (SavedPlayer i (parseRole role) lv ex gd)
      []                          -> Nothing
  pure (r >>= id)

parseRole :: String -> Role
parseRole "Archer" = Archer
parseRole _        = Warrior

showRole :: Role -> String
showRole Warrior = "Warrior"
showRole Archer  = "Archer"

showStatus :: PlayerStatus -> String
showStatus Alive        = "Alive"
showStatus Dead         = "Dead"
showStatus Disconnected = "Disconnected"

-- Kết quả của một người chơi trong một ván.
data RunPlayer = RunPlayer
  { rpDbId   :: Int
  , rpRole   :: Role
  , rpIsHost :: Bool
  , rpStatus :: PlayerStatus
  , rpGold   :: Int
  , rpExp    :: Int
  }

-- Exp cần để lên cấp tiếp theo (cấp 1: 100, cấp 2: 200, ...).
expToNext :: Int -> Int
expToNext lv = 100 * lv

-- Cộng exp, tự lên cấp nếu đủ. Trả về (level, exp còn lại).
addExp :: Int -> Int -> Int -> (Int, Int)
addExp lv ex gain
  | total >= need = addExp (lv + 1) 0 (total - need)
  | otherwise     = (lv, total)
  where
    total = ex + gain
    need  = expToNext lv

-- Lưu kết quả một ván trong MỘT transaction:
-- ghi game_sessions + session_players và cộng gold/exp/level vào players.
saveRun :: String -> UTCTime -> Result -> Int -> [RunPlayer] -> IO ()
saveRun roomCode startedAt result mapsCleared rps = do
  _ <- safely "saveRun" $ withConn $ \conn -> withTransaction conn $ do
    let hostId = case [rpDbId p | p <- rps, rpIsHost p] of
          (h : _) -> Just h
          []      -> Nothing
        resultText = case result of
          Victory -> "Victory" :: String
          Defeat  -> "Defeat"
    [Only sid] <- query conn
      "INSERT INTO game_sessions (room_code, host_player_id, started_at, ended_at, result, maps_cleared) \
      \VALUES (?, ?, ?, NOW(), ?, ?) RETURNING id"
      (roomCode, hostId, startedAt, resultText, mapsCleared) :: IO [Only Int]
    forM_ rps $ \p -> do
      cur <- query conn
        "SELECT level, exp FROM players WHERE id = ? FOR UPDATE"
        (Only (rpDbId p)) :: IO [(Int, Int)]
      case cur of
        [] -> pure ()
        ((lv0, ex0) : _) -> do
          let (lv1, ex1) = addExp lv0 ex0 (rpExp p)
              win = if result == Victory then 1 else 0 :: Int
          _ <- execute conn
            "UPDATE players SET level = ?, exp = ?, gold = GREATEST(gold + ?, 0), \
            \total_runs = total_runs + 1, total_wins = total_wins + ?, updated_at = NOW() \
            \WHERE id = ?"
            (lv1, ex1, rpGold p, win, rpDbId p)
          _ <- execute conn
            "INSERT INTO session_players \
            \(session_id, player_id, role, is_host, final_status, gold_earned, exp_earned, level_before, level_after) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
            ( sid, rpDbId p, showRole (rpRole p), rpIsHost p, showStatus (rpStatus p)
            , rpGold p, rpExp p, lv0, lv1 )
          pure ()
    putStrLn ("[DB] da luu van " ++ show sid ++ " (phong " ++ roomCode ++ ")")
  pure ()

-- Lưu gold/exp nhặt được khi người chơi thoát giữa ván (không ghi game_sessions).
saveProgress :: Int -> Int -> Int -> IO ()
saveProgress dbId gold ex = do
  _ <- safely "saveProgress" $ withConn $ \conn -> withTransaction conn $ do
    cur <- query conn "SELECT level, exp FROM players WHERE id = ? FOR UPDATE"
             (Only dbId) :: IO [(Int, Int)]
    case cur of
      [] -> pure ()
      ((lv0, ex0) : _) -> do
        let (lv1, ex1) = addExp lv0 ex0 ex
        _ <- execute conn
          "UPDATE players SET level = ?, exp = ?, gold = GREATEST(gold + ?, 0), updated_at = NOW() WHERE id = ?"
          (lv1, ex1, gold, dbId)
        putStrLn ("[DB] luu tien trinh nguoi choi " ++ show dbId)
  pure ()
