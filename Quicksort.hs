import Data.List (partition)
import Data.Bifunctor (bimap)
import Data.Semigroup (Sum(..))
import System.Random
import Data.Maybe (fromJust)
fastpart fn = go ([],[],0)
    where go (a,b,c) (x:xs)
            |fn x = go (x:a,b,c+1) xs
            |True = go (a,x:b,c+1) xs
          go tpl [] = tpl

tell :: a -> (a,b)
tell = flip (,) undefined

quicksorth :: (Num a,Ord b) => [b] -> (Sum a,[b] -> [b])
quicksorth [] = (1,id)
quicksorth [x] = (1,(x:))
quicksorth (x:xs) = do
    let (a,b,c) = fastpart (<= x) xs
    tell (1+c)
    c <- quicksorth a
    d <- quicksorth b
    return (c . (x:) . d)

quicksort :: (Num a,Ord b) => [b] -> (a, [b])
quicksort = bimap getSum ($ []) . quicksorth


sqsorth :: Ord a => [a] -> [a] -> [a]
sqsorth [] = id
sqsorth [x] = (x:)
sqsorth (x:xs) =  (sqsorth a . (x:) . sqsorth b)
    where (a,b) = partition (<= x) xs
sqsort :: Ord a => [a] -> [a]
sqsort = ($ []) . sqsorth

sssort [] = []
sssort [x] = [x]
sssort (x:xs) =  sssort a ++ (x: sssort b)
    where (a,b) = partition (<= x) xs


slowsort :: (Num a,Ord b) => [b] -> (Sum a,[b])
slowsort [] = (1,[])
slowsort (x:xs) = do
    let (a,b,c) = fastpart (<= x) xs
    tell (1+c)
    c <- slowsort a
    d <- slowsort b
    return (c ++ (x:d))





data Threebool = F | T | B
{- data GenericAtom = GenericAtom {
    number :: Int 
    mass :: Double 
    capital :: Char 
    symbol :: String
    charges :: Either Int (Int,Int)
    positive :: Threebool
}
-}
myfun :: Maybe a -> a
myfun = fromJust
myfunc :: (Functor f,Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
myfunc f a b = fmap (\x -> fmap (f x) b ) a