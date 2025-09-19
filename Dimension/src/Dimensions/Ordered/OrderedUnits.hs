{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes,DerivingStrategies #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}
module Dimensions.OrderedUnits (Dimension(..)
                    ,(!*),(!/)
                    ,Replace,Isos,Delete
                    ,Format,ValidDimension,ValidParse
                    ,mkisos,applypos,apply,same,transformpos,transform
                    ,validateDimension,undimension
                    ,dimension,dimensions,noParseimensions,noParseimension) where 
import GHC.TypeLits qualified as TL
import GHC.TypeLits (Symbol,KnownSymbol)
import Dimensions.TypeLevelInt qualified as TI
import Dimensions.TypeLevelInt (Int')
import Dimensions.Parser (Parse)
import Data.Semigroup (Endo(appEndo,Endo),stimes)
import Dimensions.Order (Sort,Merge)
import Data.Kind (Type,Constraint)
import Dimensions.DimensionalMisc (Isos',Delete,UnZero,Replace',LookupD0,Invert)
import Dimensions.Printer (Print)
import Dimensions.GetTermLevel qualified as TT 

type Replace :: k -> k -> [(k, Int')] -> [(k, Int')]
type Replace s t x = Sort (Replace' s t x)
type Isos :: [(a, a)] -> [(a, k)] -> [(k, Int')]
type Isos a b = Sort (Isos' a b)
type Format :: [(k, Int')] -> [(k, Int')]
type Format a = Sort (UnZero a)
type ValidDimension :: [(k, Int')] -> Constraint
type ValidDimension a = (a ~ Format a)
type ValidParse :: Symbol -> [(k,Int')]
type ValidParse a = Sort (Parse a)
type Dimension :: forall k. [(k,Int')] -> Type -> Type 
newtype Dimension a b = MkDimension b
    deriving stock (Eq,Ord)

--deriving newtype instance (Num b,ValidDimension a) => Num (Dimension a b) 
--deriving newtype instance (Fractional b,ValidDimension a) => Fractional (Dimension a b) 
instance (Show b,KnownSymbol (Print a)) => Show (Dimension a b) where
    show (MkDimension a) =  show a -- ++ ' ' : getString @a

dimension :: forall a b. b -> Dimension (ValidParse a) b 
dimension = MkDimension 
dimensions :: forall a f b. Functor f => f b -> f (Dimension (ValidParse a) b) 
dimensions = fmap MkDimension
noParseimension :: forall a b. b -> Dimension (Format a) b
noParseimension = MkDimension
noParseimensions :: forall a f b. Functor f => f b -> f (Dimension (Format a) b)
noParseimensions = fmap MkDimension

validateDimension :: Dimension a b -> Dimension (Format a) b
validateDimension (MkDimension a) = MkDimension a

(!*) :: Num n => Dimension a n -> Dimension b n -> Dimension (UnZero (Merge a b)) n
(MkDimension a) !* (MkDimension b) = MkDimension (a * b)
infixl 7 !*
(!/) :: Fractional n => Dimension a n -> Dimension b n -> Dimension (UnZero (Merge a (Invert b))) n
(MkDimension a) !/ (MkDimension b) = MkDimension (a * b)
infixl 7 !/

undimension :: Dimension '[] a -> a
undimension (MkDimension a) = a

transform :: forall s t x a. TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transform (fun,invfun) (MkDimension a) = let times = TT.intval @(LookupD0 s x) in
    case compare times 0 of 
        EQ -> MkDimension a
        GT -> MkDimension $ appEndo (stimes times (Endo fun)) a
        LT -> MkDimension $ appEndo (stimes (negate times) (Endo invfun)) a

transformpos :: forall s t x a. (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transformpos fun (MkDimension a) = let times = TT.natVal @(TI.ToNatural (LookupD0 s x)) in
    MkDimension $ appEndo (stimes times (Endo fun)) a

same :: forall s t x. (forall a. Dimension x a -> Dimension (Replace s t x) a)
same (MkDimension a) = MkDimension a

apply :: forall s x a.TT.ToInt (LookupD0 s x) => (a->a,a->a) -> Dimension x a -> Dimension (Delete s x) a
apply (fun,invfun) (MkDimension a) = let times = TT.intval @(LookupD0 s x) in 
    case compare times 0 of 
        EQ -> MkDimension a
        GT -> MkDimension $ appEndo (stimes times (Endo fun)) a
        LT -> MkDimension $ appEndo (stimes (negate times) (Endo invfun)) a

applypos :: forall s x a. (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a
applypos fun (MkDimension a) = let times = TT.natVal @(TI.ToNatural (LookupD0 s x)) in
    MkDimension $ appEndo (stimes times (Endo fun)) a
--mkisos is the same as repeated use of same
mkisos :: forall y x a. Dimension x a -> Dimension (Isos y x) a
mkisos (MkDimension a) = MkDimension a

--whitneyppl = dimension @"People" 2000 
--whitneyacedemicenter = dimension @"People" 400
--whitneyhs = whitneyppl - whitneyacedemicenter
--mathteamhs = dimension @"Math Teamers/People" 0.01
--mathteamac = dimension @"Math Teamers/People" (0.05 :: Double)
--mathteamers = whitneyhs !* mathteamhs + whitneyacedemicenter !* mathteamac 