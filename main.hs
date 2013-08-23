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

l1MapStr = [
	"xxxxxxxxxxxxxxxxxxx",
	"x                 x",
	"x                 x",
	"x                 x",
	"x               xxx",
	"x               xKx",
	"x               x x",
	"x               D x",
	"x               xxx",
	"x               D d",
	"x               xxx",
	"x                 x",
	"x                 x",
	"x                 x",
	"x         xx      x",
	"x         Kx      x",
	"x     xxxx xxxxxxxx",
	"x      k  D      mx",
	"xxxxxxxxxxxxxxxxxxx"]

squares m = concatMap f $ zip l1MapStr [0, m ..]
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
		objp = player 0 [0] $ square 20 (-240, 0)
		objE = solidSimpleEnemy 1 [3] Player [0] $ square 16 (240, 240)
		objF = finish [0] $ square 30 (300,0)
		objK1 = key [0, 1] World $ square 35 (-60, 240)
		objK2 = key [1, 1] Global $ square 35 (30, 180)
		objK3 = key [2, 1] Global $ square 35 (240, -120)
		objD1 = solidDoor [0, 2] Global $ square 30 (30, 240)
		objD2 = solidDoor [1, 2] World $ square 30 (270, 0)
		objD3 = solidDoor [2, 2] Global $ square 30 (210, 0)
		objD4 = solidDoor [3, 2] Global $ square 30 (210, -60)
		obj = mconcat [obj1, objE, objp, objF, objK1, objK2, objK3, objD1, objD2, objD3, objD4,
			clock Global (-600, -200), clock Player (-600, -260), clock World (-600, -320)]
		--timeconf = stdTimeConf
		timeconf = placeTimeConf [0]
		mode = InWindow "test" (100, 100) (500, 500)
