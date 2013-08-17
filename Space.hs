{-# LANGUAGE TemplateHaskell #-}

module Space where

import Control.Lens
import Graphics.Gloss
import Data.Monoid

import Types

type Space layer = layer -> Point -> Any

_x, _y :: Lens' Point Float
_x = _1
_y = _2

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
isInside r (x, y) = x >= r^.rLeft    && x <= r^.rRight
		 && y >= r^.rBottom  && y <= r^.rTop

toSpace :: (layer -> Bool) -> Rect -> Space layer
toSpace f r l = Any . if f l
		then isInside r
		else const False

morphRect t (Rect a1 a2 a3 a4) (Rect b1 b2 b3 b4) = Rect (a1%b1) (a2%b2) (a3%b3) (a4%b4)
	where a % b = a*(1-t)+b*t
