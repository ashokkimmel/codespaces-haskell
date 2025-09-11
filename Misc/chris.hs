import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Function
import Data.Void
import Control.Monad (void)
import Data.Tuple
import Data.Maybe
type Parser = Parsec Void String
type Pt = (Int,Int)
myparse :: Parser (Pt,Pt)
myparse = do 
  a <- some digitChar
  string "-"
  b <- some digitChar 
  string ","
  c <- some digitChar 
  string "-"
  d <- some digitChar 
  return ((read a, read b),(read c, read d)) 
myfun :: (Pt,Pt) -> Bool
myfun ((a,b),(c,d)) = (a >= c && d <= b)
isgood = ((||) `on` myfun) <*> swap
main = do 
  file <- readFile "input.txt"
  let x = lines file 
      y = runParser () x 
      z = map isgood y
  print z
