{-# LANGUAGE EmptyDataDecls,FunctionalDependencies,UndecidableInstances,AllowAmbiguousTypes,DefaultSignatures #-}
import Control.Applicative (liftA2)

import Data.Coerce
newtype Tonnes a = Tonnes a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Meters a = Meters a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Grams a = Grams a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Calories a = Calories a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Populations a = Populations a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Days a = Days a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Years a = Years a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Hours a = Hours a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Minutes a = Minutes a deriving(Show,Eq,Ord,Functor,Foldable,Num)
newtype Seconds a = Seconds a deriving(Show,Eq,Ord,Functor,Foldable,Num)

--type ThisCalc = Dimension 

class IsBase b  
class (IsBase b) => ToBase a b | a -> b where 
    a :: * -> * 
    b :: * -> *
    basenum :: Fractional f => f
    frombase :: Fractional f => (b f) -> (a f)
    default frombase :: (Coercible (f -> f) ((b f) -> (a f)),Fractional f) => (b f) -> (a f) 
    frombase = coerce (*basenum)
    tobase :: Fractional f => (a f) -> (b f) 
    default tobase :: (Coercible (f -> f) ((a f) -> (b f)),Fractional f) => a f -> b f 
    tobase = coerce (/basenum)
instance IsBase Seconds
instance ToBase Years Seconds where basenum = 31536000
instance ToBase Days Seconds where basenum = 86400
instance ToBase Hours Seconds where basenum = 3600
instance ToBase Minutes Seconds where basenum = 60
instance ToBase Seconds Seconds where basenum = 1 


newtype Neg f a = Neg (f a)

class Negate a b | a -> b where 
    neg :: a -> b 
    neg _ = u 
instance Negate Zero Zero 
instance Negate a b => Negate (Prev a) (Succ b) 
instance Negate a b => Negate (Succ a) (Prev b) 


class Add a b c | a b-> c where 
    add :: a -> b -> c
    add _ _ = u 

instance Add a Zero a 
instance Add Zero a a
instance Add a b c => Add (Succ a) (Prev b) c 
instance Add a b c => Add (Prev a) (Succ b) c 
instance Add a b c => Add (Succ a) (Succ b) (Succ (Succ c)) 
instance Add a b c => Add (Prev a) (Prev b) (Prev (Prev c)) 


u = undefined 
data Zero
data Succ a
data Prev a
class ToDieminsion ttype dimension 
--newtype Dimension amount mtype value  =  Dimension (mtype value) | deriving (Show,Functor,Foldable,Eq,Ord,Num)
