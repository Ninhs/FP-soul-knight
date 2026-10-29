module Config (getDatabaseUrl, getPort) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

getDatabaseUrl :: IO String
getDatabaseUrl = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> return url
    Nothing  -> return "postgresql://postgres:postgres@localhost:5432/soulknight"

getPort :: IO Int
getPort = do
  mPort <- lookupEnv "PORT"
  case mPort >>= readMaybe of
    Just p  -> return p
    Nothing -> return 8080