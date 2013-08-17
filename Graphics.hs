module Graphics where

import Types
import Space

import Graphics.Gloss

modeToCol World  = blue
modeToCol Global = green
modeToCol Player = yellow

drawR = Polygon . getPoints
