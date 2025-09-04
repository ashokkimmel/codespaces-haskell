import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Data.Maybe 
import Data.List
import Data.Bifunctor
import Data.Array
import Data.Char 
type Parser = Parsec Void String
myparse :: Parser (Int,Int,Int)
myparse = do 
  string "move "
  x <- some digitChar
  string " from "
  y <- some digitChar 
  string " to "
  z <- some digitChar 
  return (read x, read y, read z)
myrunparse x = parseMaybe myparse x 
toarray x = array (1,length x) x 
parseinput = toarray . map (second (dropWhile isSpace) . second reverse . first (read :: String -> Int) .  splitAt 1) . filter (isDigit . head) . map reverse . transpose . takeWhile (/=[])
myfun :: Bool -> Array Int [Char] -> (Int,Int,Int) -> Array Int [Char]
myfun ispart1 stack (a,b,c) = accum (flip (++)) (stack // [(b,newb)])  
                                    [(c,if ispart1 then (reverse oldb) else oldb)]
  where (oldb,newb) = splitAt a (stack ! b)
main :: IO ()
main = do
  -- Test the parser with some input
  readfile <- readFile "input.txt"
  let linedfile = lines readfile
      instructions = mapMaybe myrunparse linedfile  
      starter = parseinput linedfile
      getgood bool = foldMap singleton . fmap head . foldl' (myfun bool) starter $ instructions
  print (getgood True)
  print (getgood False)
