{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.TypeLits
import TypeLevelMisc (Length,FixSpan)
type Group :: a -> Nat -> [a] -> [[a]]
type family Group a n xs where
    Group _ _ '[] = '[]
    Group a n xs = Group' a n (FixSpan a n xs) 
type Group' :: a -> Nat -> ([a],[a]) -> [[a]]
type family Group' a n b where
    Group' a n '(b,c) = b ': Group a n c
type FixString :: Nat -> [Char] -> [[Char]]
type family FixString n s where 
    FixString n s = Group ' ' n s
