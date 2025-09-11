import Data.Maybe 
import Data.List
import Data.Monoid
import Data.Function (on)
import qualified Data.Map as Map  
import qualified Data.Set as Set  
type Place = (Sum Int,Sum Int)
type PlacedLetter = (Place,Char)
type Board = Map.Map Place Char 
type Play = (String,Place,Bool)
data GameStruct = GameStruct {board :: Board,
                              pl1t :: Bool,
                              pl1score :: Int,
                              pl2score ::  Int,
                              pl1letters :: [Char], 
                              pl2letters :: [Char], 
                              bag :: [Char]} deriving (Show)
pairs = [("EAIONRTLSU",1),("DG",2),("BCMP",3),("FHVWY",4),("K",5),("JX",8),("QZ",10)]
valueLetter1 :: Char -> Int
valueLetter1 = snd . fromMaybe ("",0) . flip find pairs . (\x y -> elem x (fst y))  

valueWord :: [PlacedLetter] -> Int
valueWord = sum . liftA2 zipWith (map valueLetter1) (map doubleletter)
addposn (a,b) (c,d) = (a+b,c+d)
makelst False = zip (repeat (Sum 0)) (map Sum [0..])
makelst True = zip (map Sum [0..]) (repeat (Sum 0)) 
letterPlace :: Play -> [PlacedLetter]
letterPlace (lst,x,bool) = zip (zipWith mappend (repeat x) (makelst bool)) lst
getLetter :: Board -> Place -> Char
getLetter lst place = fromMaybe '*' (Map.lookup place lst)
legalPlay1 :: Board -> PlacedLetter -> Bool
legalPlay1 myboard myplay = not (Map.member (fst myplay) myboard && not (snd myplay ==  myboard Map.! fst myplay))
legalPlay ::  Board -> [PlacedLetter] -> Bool
legalPlay myboard = any (legalPlay1 myboard)
translated a b c = 3
pictures = sum 
type Picture = Int 
renderBoard :: Board -> Picture
renderBoard =  pictures . map renderBoardh . Map.toList
renderBoardh ((a,b),c) = translated ((fromIntegral . getSum) a) ((fromIntegral . getSum) b) c
{-

    renderBoard :: Board -> String
    renderBoard x = unlines . rendersortedboard sortedboard $ 0 
        where sortedboard = sort x
    rendersortedboard :: Board -> Int -> [String]
    rendersortedboard _ 15 = []
    rendersortedboard lst row = makerow inrow 0:rendersortedboard nextrow (row + 1)
        where (inrow,nextrow) = span ((row ==) . snd . snd) lst 
    makerow :: Board -> Int -> String
    makerow _ 15 = []
    makerow [] m = replicate (15 - m) '*'
    makerow lst@((a, (b,_)):xs) m
        |b == m = a:makerow xs (m+1) 
        |otherwise =  '*' : makerow lst (m+1)-}
updateBoard :: Board ->[PlacedLetter] ->  Board
updateBoard a = Map.union a . Map.fromList
usedLetters :: Board -> [PlacedLetter] -> [Char]
usedLetters myboard = Map.elems . flip (Map.\\) myboard . Map.fromList 
loseletters mygame played 
    |pl1t mygame = mygame {
        pl1letters = take playedlen (bag mygame) ++ (pl1letters mygame \\ played) ,
        bag = undefined
    }
    |otherwise = mygame 
        where playedlen = length playedplayedlen
