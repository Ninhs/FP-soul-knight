module Input (InputRefs, newInputRefs, handleEvent) where

import Data.IORef
import Graphics.Gloss.Interface.IO.Game
import Protocol

data InputRefs = InputRefs
  { keyW :: IORef Bool
  , keyA :: IORef Bool
  , keyS :: IORef Bool
  , keyD :: IORef Bool
  , mousePos :: IORef (Float, Float)
  }

newInputRefs :: IO InputRefs
newInputRefs = InputRefs
  <$> newIORef False <*> newIORef False
  <*> newIORef False <*> newIORef False
  <*> newIORef (0, 0)

handleEvent :: (ClientMsg -> IO ()) -> InputRefs -> Event -> IO ()
handleEvent sendInput refs ev = do
  case ev of
    EventKey (Char 'w') st _ _ -> writeIORef (keyW refs) (isDown st)
    EventKey (Char 'a') st _ _ -> writeIORef (keyA refs) (isDown st)
    EventKey (Char 's') st _ _ -> writeIORef (keyS refs) (isDown st)
    EventKey (Char 'd') st _ _ -> writeIORef (keyD refs) (isDown st)
    EventMotion pos -> writeIORef (mousePos refs) pos
    EventKey (MouseButton LeftButton) Down _ _ -> sendAttack True
    _ -> pure ()
  sendMove
  where
    isDown Down = True
    isDown Up   = False

    sendMove = do
      w <- readIORef (keyW refs); a <- readIORef (keyA refs)
      s <- readIORef (keyS refs); d <- readIORef (keyD refs)
      mp <- readIORef (mousePos refs)
      let dx = (if d then 1 else 0) - (if a then 1 else 0)
          dy = (if w then 1 else 0) - (if s then 1 else 0)
      sendInput (InputMsg (dx, dy) mp False)

    sendAttack v = do
      mp <- readIORef (mousePos refs)
      sendInput (InputMsg (0, 0) mp v)