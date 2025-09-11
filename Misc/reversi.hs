{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
data Piece = Piece {_holder :: Int, _x :: Int, _y :: Int} deriving (Show,Eq,Ord) 
$(makeLenses ''Piece)
size :: Int
size = 8
data Board = Board { _pieces :: [Piece]
                   , _turn :: Int 
                   , _winner :: Maybe Int} deriving (Show)
$(makeLenses ''Board)
loc = lens (\(Piece _ x y) -> (x,y)) (\(Piece h _ _) (x,y) -> Piece h x y)
emptyBoard :: Board
emptyBoard = Board [] 0 Nothing

emptyPiece :: Piece -> [Piece] -> Bool 
emptyPiece (Piece _ x y) pieces = x < 0 || y < 0 || x >= size || y >= size || (null $ filter (\(Piece _ a b) -> a == x && b == y) pieces)

addPiece :: Piece -> Board -> Board
addPiece piece@(Piece _ x y) board@(Board pieces turn winner)
    | 