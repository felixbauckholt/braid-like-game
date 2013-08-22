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
		thicken x = mconcat $ map f [(0,0), (0,1), (1,0), (1, 1)]
			where f v = Line $ map (mappend v) x
