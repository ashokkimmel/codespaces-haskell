{-# OPTIONS_GHC -Wall #-}
import Debug.Trace 
import Control.Applicative (liftA2)
import Data.Function (fix)
import Control.Lens
import Control.Comonad
import Data.Functor.Classes
data Infinite a = a ::: Infinite a deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
instance Eq1 Infinite where 
    liftEq eq (x:::xs) (y:::ys) = eq x y && liftEq eq xs ys
instance Ord1 Infinite where 
    liftCompare eq (x:::xs) (y:::ys) = eq x y <> liftCompare eq xs ys


infixr 5 :::
izipWith f (a::: b) (c ::: d) = (f a c) ::: izipWith f b d 

iinterleave (a ::: b) bs = a ::: iinterleave bs b 

instance Applicative Infinite where 
    liftA2 = izipWith
    pure = irepeat
instance Monad Infinite where 
    a >>= b =  go a 0 where
        go (x ::: y) n = (b x !!! n) ::: (go y (n+1))
instance Comonad Infinite where 
      extract ~(a ::: _) = a
      extend f w@(~(x:::xs)) = f w ::: extend f xs 
ilast :: Infinite a -> a -- THIS FUNCTION WILL ALWAYS DIVERGE, BE CAREFUL, 
ilast (_ ::: a) = ilast a 
itake :: Int -> Infinite a -> [a]
itake n _ | n <=0 = []
itake n (a:::b) = a:itake (n-1) b
irepeat :: a -> Infinite a
irepeat = fix . (:::)
iiterate :: (a -> a) -> a -> Infinite a
iiterate fn = let helperfun n = n ::: helperfun (fn n) in helperfun
torecur :: (Eq a) => (a -> a) -> a -> a 
torecur fn a
    | a == ans = ans 
    | otherwise = torecur fn ans 
    where ans = fn a
toList :: Infinite a -> [a]
toList (a ::: b) = a : toList b
(!!!) :: Infinite a -> Int -> a
(x ::: _) !!! 0 = x
(_ ::: y) !!! n = y !!! (n-1)
infixl 9 !!!
listPart :: Iso (Infinite a) (Infinite b) [a] [b]
listPart = iso toList fromList -- only an iso if the original list was infinite 
fromList :: [a] -> Infinite a 
fromList x = toInfinite (cycle x) where 
    toInfinite (a:b) = a ::: toInfinite b
idiso ::Eq s => Iso' s a -> s -> Bool
idiso theiso val = (view (from theiso) . view theiso $ val) == val

