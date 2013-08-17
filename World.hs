module World where

import Types
import Time
import Space

import Data.Map (Map)
import Data.Map as M
import Graphics.Gloss
import Data.Monoid
import Control.Monad.RWS

data Entity = Entity {_eID :: ID, _eRect :: Maybe Rect} --TODO Typen

type WorldState layer = Map ID Entity
type World layer = (WorldState layer, Space layer)

type WorldM layer input s res = RWS (World layer, input) (World layer, Picture) s res

type WorldObj layer input = StdGameObj (World layer, input) (World layer, Picture)

worldMToTimeM :: WorldM layer input s () -> TimeM (World layer, input) s (World layer, Picture)
worldMToTimeM worldM = rws f
	where f r s = (w, s', ())
		where ((), s', w) = runRWS worldM r s

toObj :: TimeMode -> s -> WorldM layer input s () -> WorldObj layer input
toObj mode s worldM = simpleGameObj mode (worldMToTimeM worldM) s mempty
