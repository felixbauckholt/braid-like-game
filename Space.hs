{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Space where

import Control.Lens
import Graphics.Gloss
import Data.Monoid

import Types

type Space layer = layer -> Point -> Any

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

isInside :: Rect -> Point -> Bool
isInside r (x, y) = x > r^.rLeft    && x < r^.rRight
		 && y > r^.rBottom  && y < r^.rTop

toSpace :: (layer -> Bool) -> Rect -> Space layer
toSpace f r l = Any . if f l
		then isInside r
		else const False

fitsIn :: (Rect, layer) -> Space layer -> Bool
(r, l) `fitsIn` space = not $ getAny $ mconcat $ map (space l) $ getPoints r

morphRect t (Rect a1 a2 a3 a4) (Rect b1 b2 b3 b4) = Rect (a1%b1) (a2%b2) (a3%b3) (a4%b4)
	where a % b = a*(1-t)+b*t

move r (x, y) = rLeft +~ x $ rRight +~ x $ rTop +~ y $ rBottom +~ y $ r
