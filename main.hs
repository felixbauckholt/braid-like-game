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


stdMapStr = [
	"xxxxxxxxxxxxxxxxxxx",
	"x        x        x",
	"x xx xxx x xxx xx x",
	"x                 x",
	"x xx x xxxxx x xx x",
	"x    x   x   x    x",
	"xxxx xxx x xxx xxxx",
	"   x x       x x   ",
	"xxxx x xx xx x xxxx",
	"       x   x       ",
	"xxxx x xxxxx x xxxx",
	"   x x       x x   ",
	"xxxx x xxxxx x xxxx",
	"x        x        x",
	"x xx xxx x xxx xx x",
	"x  x           x  x",
	"xx x x xxxxx x x xx",
	"x    x   x   x    x",
	"x xxxxxx x xxxxxx x",
	"x                 x",
	"xxxxxxxxxxxxxxxxxxx"]

squares m = concatMap f $ zip stdMapStr [0, m ..]
	where f (l, y) = mapMaybe f' $ zip l [0, m ..]
		where	f' ('x', x) = Just $ square m (x, y)
			f' _         = Nothing



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
	where	obj1 = solidWalls obstacle $ map (flip move (-270, -270)) $ squares 30
		obj2 mode = solidMovingWall crushingObst mode 90 $ map (square 30)
				[(150,180), (-150,180), (-150,-180), (150,-180)]
		obj3 mode = solidMovingWall crushingObst mode 60 $ map (square 30)
				[(90,60), (-90,60), (-90,-60), (90,-60)]
		objp = player 0 [0] $ square 20 (-400, 0)
		objE = solidSimpleEnemy 1 World [0] $ square 16 (0,0)
		obj = mconcat [obj1, objE, objp, obj2 World, obj3 Player,
			clock Global (-600, -200), clock Player (-600, -260), clock World (-600, -320)]
		--timeconf = stdTimeConf
		timeconf = placeTimeConf [0]
		mode = InWindow "test" (100, 100) (500, 500)
