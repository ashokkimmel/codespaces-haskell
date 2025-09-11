module DefinedDatatypes 
(Direction
,DirectionQueue
,Player
,Game
,BigData
,dQToD
) where
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
    pl2p :: Int,
    pl1w :: Bool,
    pl2w :: Bool
    
} deriving (Show, Eq)
data BigData = BigData {
    game :: Game,
    page :: Int
} deriving (Show, Eq)
dQToD :: DirectionQueue -> Direction
dQToD (DirectionQueue d) = d
newgame :: BigData
newgame = BigData (Game (Player [(0,0)] RIGHT (NOTHING,NOTHING) False) (Player [(49,0)] LEFT (NOTHING,NOTHING) False) 0 0 False False) 0
