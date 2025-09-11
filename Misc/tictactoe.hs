{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (fold)
import Data.Monoid (Sum(..),Product(..))
import Data.Constraint (Dict(..),Constraint)
sum' :: forall f a. (Foldable f,Num a) => f a -> a 
sum' = case unsafeCoerce  (Dict @(Monoid (Sum a))) :: Dict (Monoid a) of Dict -> fold
{-# INLINE sum'  #-}
{-# SPECIALISE sum' :: Num a => [a] -> a #-}
{-# SPECIALISE sum' :: [Integer] -> Integer #-}

main :: IO ()
main = print (sum' [1,2,3]) 