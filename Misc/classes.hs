{-# LANGUAGE UndecidableInstances #-}

import Data.Function (on)
import Data.Monoid
class Demo a where
    numit :: a -> Int
instance Demo Int where 
    numit = id
    {-# INLINEABLE numit #-}
type Posn = (Int, Int)
instance  (Demo a,Demo b) => Demo (a,b) where
    numit (a,b) = numit a + numit b

instance Demo String where
    numit = length
instance Demo SInt where 
    numit (SInt a b) = numit (a,b)
data SInt = SInt Int String

class Step2 a where 
    rev :: a -> a
instance Num a => Step2 a where 
    rev = negate 
instance Step2 String where 
    rev = reverse 
class ProbThree a where 
    combo :: a -> a -> a
instance Num a => ProbThree a where 
    combo = (*)
class Bracket a where  
    myname :: a
    brack :: String -> a
class BracketOperation a where 
    brackdefault :: a
    operation :: a -> a -> a
    fromString :: String -> a
newtype IntBrack = IntBrack {getIntBrack :: Int } 
instance BracketOperation a => Bracket a where
    myname = brackdefault 
    brack a =   myname `operation` fromString a `operation` myname

instance BracketOperation String where 
    brackdefault = "MrHarris"
    operation = (++)
    fromString = id  
instance Semigroup IntBrack where
    (IntBrack a) <> (IntBrack b) = IntBrack (a+b) 
instance BracketOperation IntBrack where 
    brackdefault = IntBrack 42
    operation = (<>)
    fromString = IntBrack . read
newtype AInt = AInt {getAInt :: Int} deriving (Eq)
g = AInt . (*2)
instance Show AInt where 
    show (AInt a) = "I:" ++ show a 
instance Ord AInt where
    compare (AInt a) (AInt b) = compare a b 
class Scholar a where 
    study :: a -> Int 
instance Scholar AInt where 
    study = (+5) . getAInt 
instance Scholar String where 
    study = (*5) . length
class Join a where
    join :: a -> a -> a 
instance Semigroup a => Join a where 
    join = (<>)
    {-# INLINEABLE join #-}
instance Semigroup Int where 
    a <> b = a + b
instance Semigroup AInt where 
    (AInt a) <> (AInt b) = AInt $ a * b
class Bizzare a where 
    wierd :: a
    (<-->) :: Int -> a -> [a]
instance Bizzare Int where 
    wierd = 0
    (<-->) = replicate
instance Bizzare String where 
    wierd = ""
    (<-->) = replicate
newtype Boring = Boring {getboring :: Int}
instance Bizzare Boring where 
    wierd = Boring 1
    (<-->) = const . const . map Boring $ [2,3]