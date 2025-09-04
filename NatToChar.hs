{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
module NatToChar where
import Base64Encode
import GHC.TypeLits 
type NatToChars :: Nat -> [Char]
type family NatToChars n where
    NatToChars 0 = '[]
    NatToChars a = ShowNat (a `Mod` 64) : NatToChars (a `Div` 64)
type CharsToNat :: [Char] -> Nat
type family CharsToNat cs where
    CharsToNat '[] = 0
    CharsToNat (c ': cs) = ReadNat c + 64 * CharsToNat cs

