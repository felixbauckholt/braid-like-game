module Graphics where

import Types
import Space

import Graphics.Gloss
import Data.Monoid

modeToCol World  = blue
modeToCol Global = green
modeToCol Player = yellow

drawR = Polygon . getPoints

drawHaz r = Color red $ drawLine $ getPoints r

drawLine points = thicken $ close points
	where	close x = last x : x
		thicken x = Line x `mappend` Line (map (mappend (1, 1)) x)
