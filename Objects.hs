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
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

speed = 5

doesFit r l = fmap f ask
	where f ((_, space), _) = not $ blocked $ atRect l space r
doesDie r l = fmap f ask
	where f ((_, space), _) = deadly $ atRect l space r
getEntity id = fmap f ask
	where f ((m, _), _) = M.lookup id m

draw pic = tell (mempty, pic)
block f ap rect = tell ((mempty, toSpace f ap rect), mempty)
entity e = tell ((M.singleton (e^.eID) e, mempty), mempty)

drawAs mode = draw . Color (modeToCol mode)


doRect mode ap layers rect = do
	drawAs mode $ drawR rect
	block layers ap rect
	when (deadly ap) $ draw $ drawHaz rect


wall layers ap rect = toObj World () $ doRect World ap layers rect
walls layers ap = mconcat . map (wall layers ap)

movingWall layers ap mode steptime points = toObj mode (cycle points, steptime) $ do
	(l@(r1:r2:rst), n) <- get
	let	t = 1 - (fromIntegral n / fromIntegral steptime)
		r = morphRect t r1 r2
	doRect mode ap layers r
	put $ if n <= 0
		then (r2:rst, steptime)
		else (l, n-1)

solidMovingWall  = movingWall  $ const True
solidWall        = wall        $ const True
solidWalls       = walls       $ const True

player layer eid rect = toObj Player (Just rect) $ do
    s <- get
    case s of
      Nothing -> drawAs Player $ Text "dead"
      Just r -> do
	ui <- fmap snd ask
	let	f (k, v) = if ui k == Down then v else mempty
		vec = mconcat $ map f keys
		r_moved = move r vec
	willMove <- doesFit r_moved layer
	let	r' = if willMove then r_moved else r
	drawAs Player $ drawR r'
	entity $ Entity eid EPlayer $ Just r'
	willDie <- doesDie r' layer
	put $ if willDie then Nothing else Just r'
	where keys = [
		(SpecialKey KeyLeft,  (-s,  0)),
		(SpecialKey KeyRight, ( s,  0)),
		(SpecialKey KeyDown,  ( 0, -s)),
		(SpecialKey KeyUp,    ( 0,  s))]
		where s = speed

simpleEnemy layers layer mode pid rect = toObj mode rect $ do
	r <- get
	playerR <- fmap (maybe r $ fromMaybe r . view eRect) $ getEntity pid
	let	(pos, ownpos) = (getMiddle playerR, getMiddle r)
		vec = scaleP 0.1 $ pos `mappend` scaleP (-1) ownpos
		divide 0 = 0
		divide x = speed/x
		vec' = flip scaleP vec $ divide $ getDist (0,0) vec
		r_moved = move r vec'
	willMove <- doesFit r_moved layer
	let	r' = if willMove then r_moved else r
	doRect mode hazard layers r'
	draw $ Color cyan $ drawLine $ getPoints $
		flip move ownpos $ square 20 $ scaleP 4 vec'
	put r'

solidSimpleEnemy layer = simpleEnemy (/= layer) layer

clock mode (x, y) = toObj mode 0 $ do
	t <- get
	drawAs mode $ Translate x y $ Scale 0.5 0.5 $ Text $ show t
	put $ t+1
