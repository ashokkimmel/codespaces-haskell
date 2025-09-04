{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.Except
import Control.Monad (join)
import Control.Applicative (liftA2)
import Data.Ratio
import Data.List.Extra 
import Data.List.HT 
import Data.Maybe (fromJust)
import Data.Coerce (coerce)
import Data.Either (isLeft)
import Data.Bits (xor)
import Data.Bifunctor (second,first,bimap)
myid :: forall a. a -> a 
myid arg = arg :: a
pmap :: ([(a,Rational)] -> [(b,Rational)]) -> Prob a -> Prob b 
pmap = coerce
p1map :: ((a, Rational) -> (b, Rational)) -> Prob a -> Prob b
p1map = pmap . map 
allleft = all isLeft . runExceptT
lefts :: (EP a b) -> Prob a 
lefts = fmap getleft . runExceptT where 
    getleft ~(Left a) = a 
--import Data.Map as Map 
type EP a = ExceptT a Prob 
emap :: (f (Either a b) -> g (Either c d)) -> ExceptT a f b -> ExceptT c g d 
emap = coerce
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving (Show,Eq,Functor,Foldable)
instance Applicative Prob where 
    liftA2 fn (Prob x) (Prob y) = Prob [(fn a b,prob1 *prob2) |(a,prob1) <- x, (b,prob2) <- y]
    pure x = Prob [(x,1%1)]
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (second (*p)) innerxs
instance Monad Prob where
    m >>= f = flatten (fmap f m)
makedie x = Prob (zip [1..x] (repeat (1%x)))
proboperate2 op a b= sumprobs (liftA2 op a b)
rerolldie op = proboperate2 op <*> id
sumprobs (Prob a) = Prob [(v,sum (map snd (filter ((==v) . fst) a))) |v <- indivalues]
    where indivalues = nub (map fst a)
--survivedeath :: (Num a,Ord a) => a -> (a,a) -> Prob a -> EP Bool (a,a) 
survivedeath dc accum = ExceptT . (fmap (survivegiven dc accum))
survivegiven :: (Num a,Ord a) => a -> (a,a) -> a -> Either Bool (a,a)
survivegiven dc (a,_) _ | a >= 3 = (Left False) 
survivegiven dc (_,a) _ | a >= 3 = (Left True) 
survivegiven dc (a,b) 1 = Right (a+2,b)
survivegiven dc (a,b) 20 = Left True 
survivegiven dc (a,b) n | n >= dc = Right (a,1+b)
survivegiven dc (a,b) n  = Right (1+a,b) 
toExceptT = (ExceptT.) 
otherToX :: (Eq c,Eq a) => a -> Prob b -> (a -> b -> (Either c a)) -> Prob c
otherToX a b func = toX a b (toExceptT . fmap . func)
toX :: forall a b c. (Eq c,Eq a) => a -> Prob b -> (a -> Prob b -> (EP c a)) -> Prob c
toX accum probs func = sumprobs (helper (func accum probs))
    where
        helper :: (EP c a) -> Prob c
        helper x 
            | allleft xs =  lefts xs
            | otherwise =  (helper (newfunc xs))
            where xs =  (emap sumprobs x)
        newfunc :: (EP c a) -> (EP c a)
        newfunc = join . fmap (flip func probs)

p :: (Fractional c, Ord b, Num b) => Prob b -> b -> c
p die dc = fromRational . snd . last . getProb $ otherToX (0,0) die (survivegiven dc) 
d20 = makedie 20
d6 = makedie 6
twod6 = rerolldie (+) d6
deathroll = Prob ([(1,1%20),(8,8%20),(11,10%20),(20,1%20)])
aroll = proboperate2 max d20 d20
droll = proboperate2 min d20 d20
twod20 = rerolldie (+) d20 
makecheck :: Integer -> Prob Integer -> Prob Bool 
makecheck dc  = sumprobs . fmap (>=dc)
adddie = proboperate2 (+)
gmap fn = sumprobs . fmap fn

makelst base num = iterate (flip rem base . (+num)) num
baselist = [(0,1),(1,0),(1,1),(1,2),(2,1)]
make3 :: (Int,Int) -> [(Int,Int)]
make3 = takeWhile (/=(0,0)) . uncurry zip . ((bimap <*> id) (makelst 3)) 
finalans = sortOn length . fmap make3 $ baselist


asclistdif alla@(a:as) allb@(b:bs)
    | a `compare` b == GT = asclistdif alla bs  
    | a `compare` b == EQ = asclistdif as bs 
    | a `compare` b == LT = a : asclistdif as allb
asclistdif a [] = a
asclistdif [] _ = []

firstrow a = a:[0..a-1]++[a+1..]
myiterate :: ([a] -> a) -> a -> [a]
myiterate fn base = base : go [base] where  
    go accum = let ans = fn accum in ans : go (ans:accum) 
shownum :: Int -> String 
shownum a = shown ++ replicate (2- length shown) ' ' where 
    shown = show a
shownum3 :: Int -> String 
shownum3 a = shown ++ replicate (3- length shown) ' ' where 
    shown = show a

display2lst :: [[Int]] -> String
display2lst = init . unlines . reverse . map unwords . (map . map) shownum . take 53 . map (take 53)   
display2lst3 :: [[Int]] -> String
display2lst3 = init . unlines . map unwords . (map . map) shownum3 . take 30 . map (take 30)   

odisplay2lst = init . unlines . map unwords . (map . map) shownum . take 53 . map (take 53)   
lst2make pastelems pastlsts = ans : lst2make (insert ans pastelems) (map tail pastlsts)
    where ans = head (asclistdif [0..] (merge pastelems (sort (map head pastlsts))))
make2lsts = putStrLn . display2lst . mk2lsts
makedif = writeFile "textfile3.txt" . display2lst3 .  mapAdjacent (zipWith subtract) . mk2lsts
filemake2lsts = writeFile "textile.txt" . odisplay2lst . mk2lsts
mk2lsts = myiterate (lst2make []) . firstrow
the2lsts :: [[Int]]
the2lsts = mk2lsts (-1)
good2lsts = putStrLn . odisplay2lst . (zipWith . zipWith) xor (map repeat [0..]) $ repeat [0..] 


fac a = foldl' (*) 1 [1..a]
makelevel :: Integer -> [()]
makelevel l = do 
    x <- [0..l]
    y <- [0..(l-x)]
    let z = (l-x-y)
        ans = (fac l `div` (fac x*fac y*fac z) )
    if odd ans then return () else []
oanswers x = putStrLn ((map (show . length . makelevel) [1..]) !! x)
 
answers = (map (round . logBase 3 . fromIntegral . length . makelevel) [1..])
answers2 = map fn [1..] where 
    fn 1 = 1 
    fn x | odd x = 1 + fn (x-1)
         | even x = fn (x `div` 2)

