{-# LANGUAGE DeriveFunctor #-}

module Types where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.Trans.RWS
import Data.Monoid

data TimeDir = Forward | Pause | Backward
	deriving (Show, Read, Ord, Eq)
instance Monoid TimeDir where
	mempty = Forward
	Forward  `mappend` x        = x
	Pause    `mappend` x        = Pause
	Backward `mappend` Backward = Forward
	Backward `mappend` x        = x `mappend` Backward

data TimeMode = Global | Player | World
	deriving (Show, Read, Ord, Eq)

type TimeConf = TimeMode -> TimeDir

data GameObj dir mode input res = GameObj res 
		(input -> (mode -> dir) -> GameObj dir mode input res)
		deriving Functor

type StdGameObj = GameObj TimeDir TimeMode

type UserInput = Key -> KeyState

type GlobalGameObj = StdGameObj UserInput

type ID = [Integer]

type TimeM input s res = RWS input () s res
