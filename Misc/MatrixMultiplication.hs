{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.TypeLits hiding (type (*),type (+),type (-))
import TypeLevelInteger -- (Int',TypeNum,type (*),type (+),type (-),ToNat)
import TypeLevelMisc (ShowNat,GiveError,Stick)
type MultiplyEncrypt :: [[Nat]] -> [Nat] -> [Nat]
type family MultiplyEncrypt a b where 
    MultiplyEncrypt a b = ToNats (Multiply (Layer2ToInts a) (ToInts b))  

type Multiply :: [[Int']] -> [Int'] -> [Int']
type family Multiply xs yss where
    Multiply '[]       xs = '[]
    Multiply (y ': ys) xs = SingleRow xs y ': Multiply ys xs

type SingleRow :: [Int'] -> [Int'] -> Int'
type family SingleRow x y where 
    SingleRow '[] '[] = Pos 0
    SingleRow (x ': xs) (y ': ys) = x * y + SingleRow xs ys

type MultiplyWDisrimenant :: Int' -> [[Int']] -> [Int'] -> [Int']
type family MultiplyWDisrimenant disc xs yss where
    MultiplyWDisrimenant disc a b = DivideWDiscriminant disc (Multiply a b) 
type UnncurriedMultiplyWDiscriminant :: (Int',[[Int']]) -> [Int'] -> [Int']
type family UnncurriedMultiplyWDiscriminant a b where
    UnncurriedMultiplyWDiscriminant '(disc,mat) vec = MultiplyWDisrimenant disc mat vec

type DivideWDiscriminant :: Int' -> [Int'] -> [Int']    
type family DivideWDiscriminant a b where
    DivideWDiscriminant 0 b = Stick "Division by zero"
    DivideWDiscriminant a b = UnsafeDivideWDiscriminant a b

type UnsafeDivideWDiscriminant :: Int' -> [Int'] -> [Int']
type family UnsafeDivideWDiscriminant x y where 
    UnsafeDivideWDiscriminant x '[] = '[]
    UnsafeDivideWDiscriminant x (a ': b) = CheckNoRemainder (x `Mod` a) x `Div` a ': UnsafeDivideWDiscriminant x b

type CheckNoRemainder :: Int' -> a -> a
type family CheckNoRemainder a b where
    CheckNoRemainder a b = GiveError (AppendSymbol "The matrix had the divisor " (ShowNat a)) (IsZero a) b
type IsZero :: Int' -> Bool
type family IsZero a where 
    IsZero (Pos 0) = True
    IsZero a = False

