module Objects where

import Types
import World
import Space
import Graphics

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid
import Control.Monad.RWS
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M

doesFit r l = fmap f ask
	where f ((_, space), _) = (r, l) `fitsIn` space

draw pic = tell (mempty, pic)
block f rect = tell ((mempty, toSpace f rect), mempty)
entity e = tell ((M.singleton (e^.eID) e, mempty), mempty)

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

player layer eid rect = toObj Player rect $ do
	r <- get
	ui <- fmap snd ask
	let	f (k, v) = if ui k == Down then v else mempty
		vec = mconcat $ map f keys
		r_moved = move r vec
	willMove <- doesFit r_moved layer
	let	r' = if willMove then r_moved else r
	drawAs Player $ drawR r'
	entity $ Entity eid EPlayer $ Just r'
	put r'
	where keys = [
		(SpecialKey KeyLeft,  (-5,  0)),
		(SpecialKey KeyRight, ( 5,  0)),
		(SpecialKey KeyDown,  ( 0, -5)),
		(SpecialKey KeyUp,    ( 0,  5))]



clock mode (x, y) = toObj mode 0 $ do
	t <- get
	drawAs mode $ Translate x y $ Scale 0.5 0.5 $ Text $ show t
	put $ t+1
