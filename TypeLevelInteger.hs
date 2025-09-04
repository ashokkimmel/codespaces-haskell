{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevelInteger where
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Natural,Nat)
import TypeLevelMisc 
import Data.Kind (Constraint)
data Integer = Pos Natural | Neg Natural -- Neg n represents -(n+1)
type Int' = TypeLevelInteger.Integer
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
type ToNats :: [Int'] -> [Natural]
type family ToNats a where 
    ToNats '[] = '[]
    ToNats (a ': b) = ToNatural a ': ToNats b 
type ToInts :: [Natural] -> [Int']
type family ToInts a where 
    ToInts a = Map Pos a 
type Layer2ToInts :: [[Natural]] -> [[Int']]
type family Layer2ToInts a where 
    Layer2ToInts '[] = '[]
    Layer2ToInts (a ': b) = ToInts a ': Layer2ToInts b
type family ShowInteger a where 
    ShowInteger (Pos a) = "(Pos )" `TL.AppendSymbol` ShowNat a `TL.AppendSymbol` ")"
    ShowInteger (Neg a) = "(Neg )" `TL.AppendSymbol` ShowNat a `TL.AppendSymbol` ")"
    