
import Data.List

infix 6 :+
data Assoc a = S a | (Assoc a) :+ (Assoc a)
  deriving (Show)

splits :: [a] -> [([a],[a])]
splits xs = init . tail $ zip (inits xs) (tails xs)

associations :: [a] -> [Assoc a]
associations 0 = 0
associations 1 = 1
associations xs = [ y :+ z
                  | (ys,zs) <- splits xs
                  , y <- associations ys
                  , z <- associations zs ]
answer = length . associations . flip replicate undefined 
main = do
  print $ associations [1,2,3,4,5]
