{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
import Control.Applicative (liftA2)
instance (Bounded a,Enum a,Eq b) => Eq (a -> b) where
    a == b = all (liftA2 (==) a b) [minBound..maxBound]
instance {-# OVERLAPPABLE #-} (Num a,Applicative f) => Num (f a) where 
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    (+) = liftA2 (+)
    fromInteger = pure . fromInteger
    abs = fmap abs
    negate = fmap negate 
    signum = fmap signum
{- data Dynamic = forall a. Num a => MakeD a
instance Num Dynamic where 
    (MakeD a)-(MakeD b) = MakeD (a - b)
    (MakeD a)*(MakeD b) = MakeD (a * b)
    (MakeD a)+(MakeD b) = MakeD (a + b)
    fromInteger = MakeD . fromInteger
    abs (MakeD a) = MakeD . abs $ a
    signum (MakeD a) =MakeD . signum $ a
    negate (MakeD a) =MakeD . negate $ a
    
-}
class AType a where 
    t :: a 
    t = undefined
instance AType a 
