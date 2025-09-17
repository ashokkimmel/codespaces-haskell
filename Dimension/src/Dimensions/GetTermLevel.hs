{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications ,ScopedTypeVariables,DataKinds,AllowAmbiguousTypes #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

module Dimensions.GetTermLevel (natVal,charVal,symbolVal,intval,ToInt) where 
import GHC.TypeLits qualified as TL (natVal,symbolVal,charVal)  
import GHC.TypeLits (KnownNat,KnownChar,KnownSymbol)  
import Data.Proxy (Proxy(Proxy))
import Dimensions.TypeLevelInt (Int'(Pos,Neg))
import Data.Kind (Constraint)
natVal :: forall a. KnownNat a => Integer  
natVal = TL.natVal (Proxy @a)
charVal :: forall a. KnownChar a => Char  
charVal = TL.charVal (Proxy @a)
symbolVal :: forall a. KnownSymbol a => String  
symbolVal = TL.symbolVal (Proxy @a)

type ToInt :: Int' -> Constraint
class ToInt a where
    intval :: Integer
instance KnownNat n => ToInt ('Pos n) where
    intval = TL.natVal (Proxy :: Proxy n)
instance KnownNat n => ToInt ('Neg n) where
    intval = negate (TL.natVal (Proxy :: Proxy n)) - 1