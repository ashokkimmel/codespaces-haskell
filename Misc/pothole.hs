{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- Peano numbers
data Z
data S n

-- Bits
data B0
data B1

-- Bit list
data Nil
data Cons b bs

-- Modulo 2 (parity) at type level
type family Mod2 n where
  Mod2 Z = B0
  Mod2 (S Z) = B1
  Mod2 (S (S n)) = Mod2 n

-- Divide by 2 at type level
type family Div2 n where
  Div2 Z = Z
  Div2 (S Z) = Z
  Div2 (S (S n)) = S (Div2 n)

-- Main conversion: Peano -> Bits
type family PeanoToBits n where
  PeanoToBits Z = Nil
  PeanoToBits n = Cons (Mod2 n) (PeanoToBits (Div2 n))
