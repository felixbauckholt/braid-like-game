import Types
import Time
import World
import Space
import Objects

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

initialUserInput = const Up

onEvent :: Event -> UserInput -> UserInput
onEvent (EventKey key state _ _) ui key'
	| key == key' = state
	| otherwise   = ui key'
onEvent _ ui key'     = ui key'

mainWith :: (Display, Color, Int)
         -> GlobalGameObj (Picture, UserInput -> TimeConf)
         -> IO ()
mainWith (mode, col, tps) obj = play mode col tps initialWorld getPic event (const next)
	where	initialWorld = (obj, initialUserInput)
		getPic (GameObj (pic, _) _, _) = pic
		event e (obj, ui) = (obj, onEvent e ui)
		next (obj@(GameObj (_, timeconf) step), ui) = (step ui $ timeconf ui, ui)

tieWorldObj :: (World l -> UserInput -> TimeConf)
            -> WorldObj l UserInput 
            -> GlobalGameObj (Picture, UserInput -> TimeConf)
tieWorldObj tconf wObj = feedBack $ fmap f wObj
	where f (world, pic) = (world, (pic, tconf world))

main = mainWith (mode, black, 30) $ tieWorldObj timeconf obj
	where	obj1 = solidWalls obstacle $ [square 100 (0,0),
					square 20 (-200, -200),
					Rect 400 700 (-200) (-100)]
		obj2 mode = solidMovingWall crushingObst mode 60 $ map (square 100)
				[(100,100), (-100,100), (-100,-100), (100,-100)]
		objp = player () [0] $ square 90 (-400, 0)
		obj = mconcat [obj1, obj2 Global, obj2 World, objp,
			clock Global (-600, -200), clock Player (-600, -260), clock World (-600, -320)]
		timeconf w ui mode = case mode of
			Global -> Forward
			_      -> case ui $ SpecialKey KeySpace of
				Down -> Backward
				Up   -> Forward
		mode = InWindow "test" (100, 100) (500, 500)
