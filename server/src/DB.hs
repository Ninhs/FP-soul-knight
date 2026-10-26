{-# LANGUAGE OverloadedStrings #-}
module DB (connectDB, initSchema) where

import Database.PostgreSQL.Simple
import Config (getDatabaseUrl)
import Data.ByteString.Char8 (pack)

connectDB :: IO Connection
connectDB = do
  url <- getDatabaseUrl
  connectPostgreSQL (pack url)

initSchema :: Connection -> IO ()
initSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS players (\
    \ id SERIAL PRIMARY KEY, \
    \ name TEXT NOT NULL, \
    \ score INT NOT NULL DEFAULT 0, \
    \ created_at TIMESTAMP DEFAULT NOW())"
  return ()