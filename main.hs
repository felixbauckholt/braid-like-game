import Types
import Time
import World
import Space
import Objects

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid
import Control.Lens
import Data.Maybe
import qualified Data.Map as M

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

stdTimeConf w ui = f
	where	f Global = ifPressed (Char 'p') Pause
		f Player = f Global `mappend` ifPressed (SpecialKey KeySpace)  Backward
		f World  = f Player
		pressed = (== Down) . ui
		ifPressed k dir = if pressed k then dir else mempty

placeTimeConf pid (ws, _) ui = f
	where	f Global = ifPressed (Char 'p') Pause
		f Player = f Global `mappend` ifPressed (SpecialKey KeySpace)  Backward
		f World  = f Player `mappend` fromMaybe Pause playerMovement
		pressed = (== Down) . ui
		ifPressed k dir = if pressed k then dir else mempty
		playerMovement = do
			e <- M.lookup pid ws
			(x, y) <- case e^.eType of
				EPlayer pd -> Just $ pd^.lastMovement
				_          -> Nothing
			toDir x
			where toDir x
				| x == 0        = Just Pause
				| x == speed    = Just Forward
				| x == (-speed) = Just Backward
				| otherwise     = Nothing

main = mainWith (mode, black, 30) $ tieWorldObj timeconf obj
	where	obj1 = solidWalls obstacle $ [square 100 (0,0),
					square 20 (-200, -200),
					Rect 400 700 (-200) (-100)]
		obj2 mode = solidMovingWall crushingObst mode 60 $ map (square 100)
				[(100,100), (-100,100), (-100,-100), (100,-100)]
		objp = player 0 [0] $ square 90 (-400, 0)
		objE = solidSimpleEnemy 1 World [0] $ square 50 (400,0)
		obj = mconcat [obj1, obj2 Global, obj2 World, objp, objE,
			clock Global (-600, -200), clock Player (-600, -260), clock World (-600, -320)]
		--timeconf = stdTimeConf
		timeconf = placeTimeConf [0]
		mode = InWindow "test" (100, 100) (500, 500)
