{-# LANGUAGE DeriveFunctor #-}

module Types where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.Trans.RWS

data TimeDir = Forward | Pause | Backward
	deriving (Show, Read, Ord, Eq)

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
