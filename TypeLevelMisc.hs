{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevelMisc (ShowNat,GetElem,SetElem,GiveError,IfThenElse,Length,FixSpan,Span,Repeat,type (++),Stick,Map) where
import GHC.TypeLits
type ShowNat :: Nat -> Symbol
type family ShowNat a where
    ShowNat 0 = "0"
    ShowNat 1 = "1"
    ShowNat 2 = "2"
    ShowNat 3 = "3"
    ShowNat 4 = "4"
    ShowNat 5 = "5"
    ShowNat 6 = "6"
    ShowNat 7 = "7"
    ShowNat 8 = "8"
    ShowNat 9 = "9"
    ShowNat a = AppendSymbol (ShowNat (a `Div` 10)) (ShowNat (a `Mod` 10))
type GetElem :: Nat -> [a] -> a
type family GetElem n xs where
    GetElem 0 (x ': xs) = x
    GetElem n (x ': xs) = GetElem (n - 1) xs
type SetElem :: Nat -> a -> [a] -> [a]
type family SetElem n a xs where
    SetElem 0 a (x ': xs) = a ': xs
    SetElem n a (x ': xs) = x ': SetElem (n - 1) a xs
type GiveError :: Symbol -> Bool -> a -> a 
type family GiveError a b c where 
    GiveError _ True a = a 
    GiveError s False _ = Stick s
type IfThenElse :: Bool -> a -> a -> a
type family IfThenElse cond a b where
    IfThenElse True a b = a
    IfThenElse False a b = b
type Length :: [a] -> Nat
type family Length xs where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs
type FixSpan :: a -> Nat -> [a] -> ([a],[a])
type family FixSpan a n xs where
    FixSpan _ 0 xs = '( '[], xs)
    FixSpan a n '[] = '(Repeat n a,'[])
    FixSpan a n (x ': xs) = Add x (FixSpan a (n - 1) xs)
type Repeat :: Nat -> a -> [a]
type family Repeat n a where
    Repeat 0 a = '[]
    Repeat n a = a ': Repeat (n - 1) a
type Add :: a -> ([a],b) -> ([a],b)
type family Add a b where 
    Add a '(d,c) = '(a ': d,c)
type Span :: Nat -> [a] -> ([a],[a])
type family Span n xs where
    Span 0 xs = '( '[], xs)
    Span n (x ': xs) = Add x (Span (n - 1) xs)
type (++) :: [a] -> [a] -> [a]
type family (++) a b where
    '[] ++ b = b
    (x ': xs) ++ b = x ': (xs ++ b)
infixr 5 ++
type Stick :: Symbol -> a
type family Stick where 
type Reverse :: [a] -> [a]
type family Reverse xs where
    Reverse a = Reverse' a '[]
type Reverse' :: [a] -> [a] -> [a]
type family Reverse' xs acc where
    Reverse' '[] acc = acc
    Reverse' (x ': xs) acc = Reverse' xs (x ': acc)
type  Map :: (a -> b) -> [a] -> [b] 
type family Map a b where 
    Map _ '[] = '[] 
    Map f (a ': b) = f a ': Map f b