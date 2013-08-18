{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Space where

import Control.Lens
import Graphics.Gloss
import Data.Monoid

import Types


type AtPoint = (Any, Any)
type Space layer = ([(Point, AtPoint)], layer -> Point -> AtPoint)

obstacle, hazard, crushingObst :: AtPoint
obstacle = (Any True, mempty)
hazard   = (mempty, Any True)
crushingObst = obstacle `mappend` hazard

_x, _y :: Lens' Point Float
_x = _1
_y = _2

instance Monoid Point where
	mempty = (0, 0)
	(x1, y1) `mappend` (x2, y2) = (x1+x2, y1+y2)

data Rect = Rect {_rLeft, _rRight, _rBottom, _rTop :: Float}
	deriving (Show, Read, Eq)
makeLenses ''Rect

getPoints :: Rect -> [Point]
getPoints r = map (both %~ (r^.)) lenses
	where lenses = [
		(rLeft,  rTop   ),
		(rRight, rTop   ),
		(rRight, rBottom),
		(rLeft,  rBottom)]

getMiddle :: Rect -> Point
getMiddle r = ((r^.rLeft+r^.rRight)/2, (r^.rBottom+r^.rTop)/2)

square :: Float -> Point -> Rect
square r' (x, y) = Rect (x-r) (x+r) (y-r) (y+r)
	where r = r'/2

isInside :: Rect -> Point -> Bool
isInside r (x, y) = x > r^.rLeft    && x < r^.rRight
		 && y > r^.rBottom  && y < r^.rTop

toSpace :: (layer -> Bool) -> AtPoint -> Rect -> Space layer
toSpace layers ap r = (map f2 $ getPoints r, f1)
	where	f1 l p = if layers l && isInside r p
			then ap
			else mempty
		f2 p = (p, ap)

blocked (Any x, _) = x
deadly  (_, Any x) = x

atPoint :: layer -> Space layer -> Point -> AtPoint
atPoint layer (_, f) p = f layer p

atRect :: layer -> Space layer -> Rect -> AtPoint
atRect l (ps, f) r =  mconcat $ map (f l) (getPoints r)
		   ++ map snd (filter (isInside r . fst) ps)

morphRect t (Rect a1 a2 a3 a4) (Rect b1 b2 b3 b4) = Rect (a1%b1) (a2%b2) (a3%b3) (a4%b4)
	where a % b = a*(1-t)+b*t

move r (x, y) = rLeft +~ x $ rRight +~ x $ rTop +~ y $ rBottom +~ y $ r
