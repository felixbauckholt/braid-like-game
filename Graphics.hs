module Graphics where

import Types
import Space

import Graphics.Gloss
import Data.Monoid

modeToCol World  = blue
modeToCol Global = green
modeToCol Player = yellow

drawR = Polygon . getPoints

drawHaz r = Color red $ Line $ thicken $ close $ getPoints r
	where	close x = last x : x
		thicken x = x ++ map (mappend (1, 1)) x
