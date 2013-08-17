module Time where

import Control.Monad.RWS
import Control.Applicative

import Types

toRWS = rws

baseGameObj :: mode -> TimeM input s res -> [(s, res)] -> GameObj TimeDir mode input res
baseGameObj mode rws last@((s, res):prev) = GameObj res f
	where f input tconf = case tconf mode of
			Pause    -> newObj last
			Forward  -> newObj $ (s', res'):last
			Backward -> newObj prev
		where	newObj = baseGameObj mode rws
			(res', s', _) = runRWS rws input s

simpleGameObj :: mode -> TimeM input s res -> s -> res -> GameObj TimeDir mode input res
simpleGameObj mode rws firstS res = baseGameObj mode (toRWS rws') $ zip ss $ repeat res
	where	ss = map Left $ [0..]
		rws' r s = case s of
			Right s -> let (a, s', ()) = runRWS rws r s      in (a, Right s', ())
			Left 0  -> let (a, s', ()) = runRWS rws r firstS in (a, Right s', ())
			Left x  -> (res, Left $ x-1, ())

instance Applicative (GameObj dir mode input) where
	pure res = GameObj res $ const $ const $ pure res
	GameObj ra fa <*> GameObj rb fb = GameObj (ra rb) f
		where f input tconf = fa input tconf <*> fb input tconf

instance Monoid res => Monoid (GameObj dir mode input res) where
	mempty = pure mempty
	ga `mappend` gb = fmap mappend ga <*> gb

feedBack :: GameObj dir mode (loop, input) (loop, res) -> GameObj dir mode input res
feedBack (GameObj (loop, res) f) = GameObj res f'
	where f' input tconf = feedBack $ f (loop, input) tconf
