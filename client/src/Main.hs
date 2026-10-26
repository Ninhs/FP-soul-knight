module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Network (NetworkConnection(..), connectToServer)
import Render (renderGame)
import Input (newInputRefs, handleEvent)

main :: IO ()
main = do
  putStrLn "Nhap ma phong:"
  code <- getLine
  putStrLn "Nhap ten nhan vat:"
  name <- getLine

  connectionResult <- connectToServer "127.0.0.1" 8080 code name
  connection <- case connectionResult of
    Left err -> do
      hPutStrLn stderr ("Khong the vao phong: " ++ err)
      exitFailure
    Right connected -> pure connected
  inputRefs <- newInputRefs

  playResult <- try $ playIO
    (InWindow "Soul Knight Mini" (800, 600) (100, 100))
    black
    60
    (initialGameState connection)
    (\gs -> pure (renderGame gs))
    (\ev gs -> handleEvent (sendClientMessage connection) inputRefs ev >> pure gs)
    (\_dt gs -> do
        mFailure <- tryTakeMVar (connectionFailure connection)
        case mFailure of
          Just err -> ioError (userError ("Mat ket noi server: " ++ err))
          Nothing -> do
            mNew <- tryTakeMVar (latestGameState connection)
            case mNew of
              Just s  -> pure s
              Nothing -> pure gs)
    :: IO (Either SomeException ())
  case playResult of
    Left err -> hPutStrLn stderr ("Client dung do loi: " ++ show err) >> exitFailure
    Right () -> pure ()