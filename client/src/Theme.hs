module Theme where

import Graphics.Gloss

-- Shared dark-fantasy palette for frontend views and components.
cPanelBg :: Color
cPanelBg = makeColor 0.025 0.045 0.09 0.96

cDarkBorder :: Color
cDarkBorder = makeColor 0.035 0.08 0.15 1.0

cPanelHighlight :: Color
cPanelHighlight = makeColor 0.055 0.11 0.19 1.0

cOverlay :: Color
cOverlay = makeColor 0.02 0.02 0.04 0.72

cPanelBorder :: Color
cPanelBorder = makeColor 0.18 0.58 0.95 1.0

cPanelGlow :: Color
cPanelGlow = makeColor 0.24 0.72 1.0 1.0

cPrimaryGold :: Color
cPrimaryGold = makeColor 0.92 0.72 0.20 1.0

cHoverGold :: Color
cHoverGold = makeColor 1.0 0.86 0.34 1.0

cTextGold :: Color
cTextGold = makeColor 1.0 0.84 0.30 1.0

cTextWhite :: Color
cTextWhite = makeColor 0.92 0.92 0.95 1.0

cTextMuted :: Color
cTextMuted = makeColor 0.62 0.64 0.70 1.0

cAccentRed :: Color
cAccentRed = makeColor 0.85 0.22 0.22 1.0

cHealthRed :: Color
cHealthRed = makeColor 0.88 0.20 0.24 1.0

cAccentBlue :: Color
cAccentBlue = makeColor 0.18 0.55 0.95 1.0

cArmorBlue :: Color
cArmorBlue = makeColor 0.30 0.68 0.95 1.0

cAccentGreen :: Color
cAccentGreen = makeColor 0.22 0.80 0.38 1.0

screenWidth, screenHeight :: Float
screenWidth = 1280
screenHeight = 720

buttonHeight, panelBorderWidth, dungeonTileSize :: Float
buttonHeight = 52
panelBorderWidth = 3
dungeonTileSize = 32