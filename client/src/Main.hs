module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.MVar

import Network (connectToServer)
import Render (renderGame)
import Input (newInputRefs, handleEvent)
import Types.GameState (GameState(..), RoomPhase(Lobby))

main :: IO ()
main = do
  putStrLn "Nhap ma phong:"
  code <- getLine
  putStrLn "Nhap ten nhan vat:"
  name <- getLine

  (stateVar, sendInput) <- connectToServer "127.0.0.1" 8080 code name
  inputRefs <- newInputRefs

  let initial = GameState Lobby []

  playIO
    (InWindow "Soul Knight Mini" (800, 600) (100, 100))
    black
    60
    initial
    (\gs -> pure (renderGame gs))
    (\ev gs -> handleEvent sendInput inputRefs ev >> pure gs)
    (\_dt gs -> do
        mNew <- tryTakeMVar stateVar
        case mNew of
          Just s  -> pure s
          Nothing -> pure gs)