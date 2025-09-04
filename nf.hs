import Data.List (elemIndex)
import Data.Maybe (fromJust)
--import DefinedDatatypes I don't know why this doesn't work but it doesn't 
data Direction = UP | LEFT | DOWN | RIGHT deriving (Show, Eq,Enum)
data DirectionQueue = DirectionQueue Direction | NOTHING deriving (Show, Eq)
data Player = Player {
    spots :: [(Int,Int)],
    dir :: Direction,
    queue :: (DirectionQueue,DirectionQueue),
    out :: Bool
} deriving (Show, Eq)
data Game = Game {
    pl1 :: Player,
    pl2 :: Player,
    pl1p :: Int,
    pl2p :: Int
} deriving (Show, Eq)
dQToD :: DirectionQueue -> Direction
dQToD (DirectionQueue d) = d
newgame :: BigData
newgame = BigData (Game (Player [(0,0)] RIGHT (NOTHING,NOTHING) 0) (Player [(49,0)] LEFT (NOTHING,NOTHING) 0)) 0
pl1strs = ["W","A","S","D"]
pl2strs = ["UP","LEFT","","D"]
changedir :: BigData -> String -> BigData
changedir myGame str = myGame {game = changedirgame (game myGame) str} 
changedirgame :: Game -> String -> Game
changedirgame myGame str 
    |str `elem` pl1strs = myGame {
        pl1 = changedirHelper (pl1 myGame) (toEnum . fromJust . elemIndex str $ pl1strs)}
    |str `elem` pl2strs = myGame {
        pl2 = changedirHelper (pl2 myGame) (toEnum . fromJust . elemIndex str $ pl1strs)}
    |otherwise = myGame

oppositeDirection :: Direction -> Direction->Bool
oppositeDirection d1  = (== toEnum ((fromEnum d1 + 2) `mod` 4))
changedirHelper :: Player -> Direction -> Player
changedirHelper tplayer newDir
    |fs == NOTHING = 
        if oppositeDirection ogDir newDir
            then tplayer
            else tplayer {
                queue = (DirectionQueue newDir,NOTHING) 
            }
    |sn == NOTHING = 
        if oppositeDirection (dQToD fs) newDir
            then tplayer
            else tplayer {
                queue = (fs,DirectionQueue newDir) 
            }
    |otherwise = tplayer  
    where
        (fs,sn) = queue tplayer
        ogDir = dir tplayer
tickHandler :: Game -> Game
tickHandler ogGame = ogGame {
    pl1 = tickPlayer (pl1 ogGame) (spots (pl1 ogGame) ++ spots (pl2 ogGame)),
    pl2 = tickPlayer (pl2 ogGame) (spots (pl1 ogGame) ++ spots (pl2 ogGame))
}
move :: (Int,Int) -> Direction -> (Int,Int)
move (x,y) UP = (x,y + 1)
move (x,y) RIGHT = (x+1,y)
move (x,y) DOWN = (x,y-1)
move (x,y) LEFT = (x-1,y)
tickPlayer :: Player -> [(Int,Int)] -> Player
tickPlayer (snake cuDir (fs,sn) _) tailspots
    |newspot `elem` tailspots = Player (newspot : snake) cuDir (fs,sn) true
    |otherwise = 
    where newspot = move (head snake) cuDir
