module Input
  ( InputRefs
  , newInputRefs
  , handleEvent
  , movementDirection
  , aimPosition
  , takeAttackRequest
  ) where

import Data.IORef
import Graphics.Gloss.Interface.IO.Game
import Protocol

data InputRefs = InputRefs
  { keyW :: IORef Bool
  , keyA :: IORef Bool
  , keyS :: IORef Bool
  , keyD :: IORef Bool
  , mousePos :: IORef (Float, Float)
  , mouseLeft :: IORef Bool
  , attackRequest :: IORef Bool
  }

newInputRefs :: IO InputRefs
newInputRefs = InputRefs
  <$> newIORef False <*> newIORef False
  <*> newIORef False <*> newIORef False
  <*> newIORef (0, 0) <*> newIORef False <*> newIORef False

movementDirection :: InputRefs -> IO (Float, Float)
movementDirection refs = do
  movingUp <- readIORef (keyW refs)
  movingLeft <- readIORef (keyA refs)
  movingDown <- readIORef (keyS refs)
  movingRight <- readIORef (keyD refs)
  pure
    ( (if movingRight then 1 else 0) - (if movingLeft then 1 else 0)
    , (if movingUp then 1 else 0) - (if movingDown then 1 else 0)
    )

aimPosition :: InputRefs -> IO (Float, Float)
aimPosition = readIORef . mousePos

takeAttackRequest :: InputRefs -> IO Bool
takeAttackRequest refs = do
  requested <- readIORef (attackRequest refs)
  writeIORef (attackRequest refs) False
  pure requested

handleEvent :: (ClientMsg -> IO ()) -> InputRefs -> Event -> IO ()
handleEvent sendInput refs event = do
  case event of
    EventKey key state _ position -> do
      updateMovement key state
      case key of
        MouseButton button -> do
          writeIORef (mousePos refs) position
          if button == LeftButton then updateAttack state else pure ()
        _ -> pure ()
    EventMotion position -> writeIORef (mousePos refs) position
    _ -> pure ()
  sendCurrentInput
  where
    isDown Down = True
    isDown Up = False

    updateAttack state = do
      wasDown <- readIORef (mouseLeft refs)
      let nowDown = isDown state
      if nowDown && not wasDown
        then writeIORef (attackRequest refs) True
        else pure ()
      writeIORef (mouseLeft refs) nowDown

    updateMovement key state =
      case key of
        Char 'w' -> writeIORef (keyW refs) (isDown state)
        Char 'W' -> writeIORef (keyW refs) (isDown state)
        Char 'a' -> writeIORef (keyA refs) (isDown state)
        Char 'A' -> writeIORef (keyA refs) (isDown state)
        Char 's' -> writeIORef (keyS refs) (isDown state)
        Char 'S' -> writeIORef (keyS refs) (isDown state)
        Char 'd' -> writeIORef (keyD refs) (isDown state)
        Char 'D' -> writeIORef (keyD refs) (isDown state)
        SpecialKey KeyUp -> writeIORef (keyW refs) (isDown state)
        SpecialKey KeyLeft -> writeIORef (keyA refs) (isDown state)
        SpecialKey KeyDown -> writeIORef (keyS refs) (isDown state)
        SpecialKey KeyRight -> writeIORef (keyD refs) (isDown state)
        _ -> pure ()

    sendCurrentInput = do
      direction <- movementDirection refs
      aim <- readIORef (mousePos refs)
      attacking <- readIORef (mouseLeft refs)
      sendInput (InputMsg direction aim attacking)
