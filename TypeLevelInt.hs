{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module TypeLevelInt where
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Natural,Nat)
import Data.Kind (Constraint)
import Data.Data (Proxy (..))
data Integer = Pos Natural | Neg Natural -- Neg n represents -(n+1)
type Int' = TypeLevelInt.Integer
type (+) :: Int' -> Int' -> Int'
type family (+) (m :: Int') (n :: Int') :: Int' where
    (+) (Pos m) (Pos n) = Pos (m TL.+ n)
    (+) (Neg m) (Neg n) = Neg (m TL.+ n TL.+ 1)
    (+) (Pos m) (Neg n) = IfThenElse (m TL.<=? n) (Neg (n TL.- m)) (Pos (m TL.- n TL.- 1))
    (+) (Neg m) (Pos n) = IfThenElse (n TL.<=? m) (Neg (m TL.- n)) (Pos (n TL.- m TL.- 1))
type (-) :: Int' -> Int' -> Int'
type family (-) (m :: Int') (n :: Int') :: Int' where
    (-) (Pos m) (Pos n) = IfThenElse (n TL.<=? m) (Pos (m TL.- n)) (Neg (n TL.- m TL.- 1))
    (-) (Neg n) (Neg m) = IfThenElse (n TL.<=? m) (Pos (m TL.- n)) (Neg (n TL.- m TL.- 1))
    (-) (Pos m) (Neg n) = Pos (m TL.+ n TL.+ 1)
    (-) (Neg m) (Pos n) = Neg (m TL.+ n)
type ToNatural :: Int' -> Nat
type family ToNatural (n :: Int') :: Nat where
    ToNatural (Pos m) = m
type (*) :: Int' -> Int' -> Int'
type family (*) m n where
    (*) (Pos 0) _ = Pos 0
    (*) _ (Pos 0) = Pos 0
    (*) (Pos m) (Pos n) = Pos (m TL.* n)
    (*) (Neg m) (Neg n) = Pos ((m TL.+ 1) TL.* (n TL.+ 1))
    (*) (Pos m) (Neg n) = Neg (m TL.* (n TL.+ 1) TL.- 1)
    (*) (Neg n) (Pos m) = Neg (m TL.* (n TL.+ 1) TL.- 1)
type IfThenElse :: Bool -> a -> a -> a
type family IfThenElse cond a b where
    IfThenElse True a b = a
    IfThenElse False a b = b

type ShowNat :: Nat -> TL.Symbol
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
    ShowNat a = TL.AppendSymbol (ShowNat (a `TL.Div` 10)) (ShowNat (a `TL.Mod` 10))
type family ShowInteger a where
    ShowInteger (Pos a) = "(Pos " `TL.AppendSymbol` ShowNat a `TL.AppendSymbol` ")"
    ShowInteger (Neg a) = "(Neg " `TL.AppendSymbol` ShowNat a `TL.AppendSymbol` ")"
class ToInt a where
    intval :: Prelude.Integer
instance TL.KnownNat n => ToInt (Pos n) where
    intval = TL.natVal (Proxy :: Proxy n)
instance TL.KnownNat n => ToInt (Neg n) where
    intval = negate (TL.natVal (Proxy :: Proxy n)) - 1
--class ToInt a => ToInt (Just a) where 
--    intval = intval @a 