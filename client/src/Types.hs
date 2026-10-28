module Types
  ( ViewState(..)
  , ClientState(..)
  ) where

import Graphics.Gloss (Picture)
import Types.GameState (GameState)

data ViewState
  = MainMenuView
  | LobbyView
  | InGameView
  deriving (Eq, Show)

data ClientState = ClientState
  { appAssets :: [(String, Picture)]
  , appView :: ViewState
  , sharedState :: GameState
  , mousePos :: (Float, Float)
  }
