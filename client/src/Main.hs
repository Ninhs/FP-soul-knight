module Main where

import Control.Exception (SomeException, try)
import System.Exit (exitSuccess)
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.IO.Game
  ( Event(..)
  , Key(..)
  , KeyState(..)
  , MouseButton(..)
  , SpecialKey(..)
  , playIO
  )
import Assets (loadAssets)
import ClientUI (UiAction(..), UiResult(..), reduceUi)
import ClientGameplay (stepLocalGameplay)
import Input
  ( InputRefs
  , handleEvent
  , movementDirection
  , newInputRefs
  , takeAttackRequest
  )
import Render (renderClient)
import Types

main :: IO ()
main = runLocalClient

runLocalClient :: IO ()
runLocalClient = do
  assets <- loadAssets
  inputRefs <- newInputRefs
  let window = InWindow "Soul Knight Haskell" (1280, 720) (100, 100)
      initialState = initialClientState assets
  playIO window black 60 initialState
    (pure . renderClient)
    (handleInput inputRefs)
    (updateLocalPreview inputRefs)

handleInput :: InputRefs -> Event -> ClientState -> IO ClientState
handleInput inputRefs event state = do
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
    EventKey (MouseButton LeftButton) Down _ (x, y) ->
      applyUiAction inputRefs (UiClick (x, y)) positionedState
    _ -> pure positionedState

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

updateLocalPreview :: InputRefs -> Float -> ClientState -> IO ClientState
updateLocalPreview inputRefs deltaTime state
  | appView state /= InGameView = do
      _ <- takeAttackRequest inputRefs
      pure state
  | otherwise = do
      direction <- movementDirection inputRefs
      attackPressed <- takeAttackRequest inputRefs
      pure (stepLocalGameplay deltaTime direction attackPressed state)