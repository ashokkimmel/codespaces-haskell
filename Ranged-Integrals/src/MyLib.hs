{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module MyLib (RangedLen'(..),RangedLen) where
import GHC.TypeLits (type (-),Nat)
import Numeric.Natural ( Natural ) 
import Data.Kind (Type,Constraint)
type ToPeano :: Nat -> Peano
type family ToPeano a where
    ToPeano 0 = Zero
    ToPeano a = Succ (ToPeano (a - 1))

data RangedLen' a where
    IsZero :: RangedLen' a
    Plus1 :: RangedLen' a -> RangedLen' ('Succ a)
type KnownPeano :: Peano -> Constraint
class KnownPeano a where
    peanoVal :: Natural
instance KnownPeano Zero where peanoVal = 0
instance KnownPeano a => KnownPeano (Succ a) where
    peanoVal = 1 + (peanoVal @a)
type RangedLen' :: Peano -> Type
fromnum :: RangedLen' a -> Int 
fromnum IsZero = 0
fromnum (Plus1 a) = fromnum a + 1 
instance Show (RangedLen' a) where 
    show a = show $ fromnum a

instance Eq (RangedLen' a) where 
    IsZero == IsZero = True 
    (Plus1 a) == (Plus1 b) = a == b 
    _ == _ = False 
instance Ord (RangedLen' a) where 
    IsZero `compare` IsZero = EQ 
    IsZero `compare` _ = LT
    _ `compare` IsZero = GT
    (Plus1 a) `compare` (Plus1 b) = a `compare` b 
instance Bounded (RangedLen' Zero) where 
    maxBound = IsZero 
    minBound = IsZero 
instance Bounded (RangedLen' a) => Bounded (RangedLen' (Succ a)) where 
    minBound = IsZero 
    maxBound = Plus1 maxBound

addBound :: RangedLen' a -> RangedLen' (Succ a)
addBound IsZero = IsZero 
addBound (Plus1 a) = Plus1 (addBound a)

type RangedLen a = RangedLen' (ToPeano a)
instance Num (RangedLen' Zero) where
    fromInteger 0 = IsZero
    fromInteger a
        | a < 0 = errorWithoutStackTrace "FromInteger: negative integral supplied, this is not allowed."
        | otherwise = errorWithoutStackTrace $ "Fromintegral argument out of range of RangedLen, given " ++ show a
    (+) _ _ = IsZero
    (-) _ _ = IsZero
    (*) _ _ = IsZero
    negate _ = IsZero
    abs _ = IsZero
    signum _ = IsZero
instance (Num (RangedLen' a), KnownPeano (Succ a), Bounded (RangedLen' (Succ a))) => Num (RangedLen' (Succ a)) where
    fromInteger a
        | a == fromIntegral (peanoVal @(Succ a)) = maxBound
        | otherwise = addBound $ fromInteger a
--    (+) IsZero a = a
--    (+) (Plus1 b) a = (Plus1 (sub1 (a + b)))
--
--    (-) _ _ = IsZero
--    (*) _ _ = IsZero
--    negate _ = IsZero
--    abs _ = IsZero
--    signum _ = IsZero
data Peano = Zero | Succ Peano

myfun :: RangedLen 1 ->  Bool 
myfun IsZero = True  
myfun (Plus1 IsZero) = False  
