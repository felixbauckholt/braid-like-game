module Objects where

import Types
import World
import Space
import Graphics

import Graphics.Gloss
import Data.Monoid
import Control.Monad.RWS

draw pic = tell (mempty, pic)
block f rect = tell ((mempty, toSpace f rect), mempty)

drawAs mode = draw . Color (modeToCol mode)


wall layers rect = toObj World () $ do
	drawAs World $ drawR rect
	block layers rect

walls layers = mconcat . map (wall layers)

movingWall layers mode points steptime = toObj mode (cycle points, steptime) $ do
	(l@(r1:r2:rst), n) <- get
	let	t = 1 - (fromIntegral n / fromIntegral steptime)
		r = morphRect t r1 r2
	block layers r
	drawAs mode $ drawR r
	put $ if n <= 0
		then (r2:rst, steptime)
		else (l, n-1)

solidMovingWall  = movingWall  $ const True
solidWall        = wall        $ const True
solidWalls       = walls       $ const True




clock mode (x, y) = toObj mode 0 $ do
	t <- get
	drawAs mode $ Translate x y $ Scale 0.5 0.5 $ Text $ show t
	put $ t+1
