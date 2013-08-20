{-# LANGUAGE TemplateHaskell #-}

module World where

import Types
import Time
import Space

import Data.Map (Map)
import qualified Data.Map as M
import Graphics.Gloss
import Data.Monoid
import Control.Monad.RWS
import Control.Lens

data PlayerData = PlayerData {_lastMovement :: Point}
	deriving (Show, Read, Eq)
makeLenses ''PlayerData

data EType = EPlayer PlayerData
	deriving (Show, Read, Eq)

data Entity = Entity {_eID :: ID, _eType :: EType, _eRect :: Maybe Rect}
	deriving (Show, Read, Eq)
makeLenses ''Entity

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
