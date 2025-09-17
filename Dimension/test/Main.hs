{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies,DataKinds,UndecidableInstances,AllowAmbiguousTypes #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
module Main (main) where
import Dimensions.Parser 
import GHC.TypeLits qualified as TL 
import Dimensions.TypeNats qualified as TL 

type (<~~>) :: a -> a -> Constraint -- General checking type equality with custom error message
type family (<~~>) a b where 
    a <~~> a = ()
    a <~~> b = TypeError ('Text "Expected " ':<>: 'TL.ShowType a 
                         ':$$: 'Text " but got " ':<>: 'ShowType b)

type (<~>) :: Symbol -> [(Symbol, Int')] -> Constraint -- Specialized for parse
type family (<~>) a b where 
    a <~> b = SpecializedParseCheck a (Parse a) b
type SpecializedParseCheck :: Symbol -> [(k, Int')] -> [(k, Int')] -> Constraint
type family SpecializedParseCheck a b c where 
    SpecializedParseCheck _ b b = ()
    SpecializedParseCheck a b c = TypeError ('Text "Parsing error: Expected " ':<>: 'TL.ShowType c
                                            ':$$: 'Text " but got " ':<>: 'ShowType b 
                                            ':$$: 'Text " when parsing the string " ':<>: 'TL.ShowType a)
type Tests :: Constraint
type Tests = 
    ( "m" <~> '[ '("m", TI.ToPosInt 1)]
    , "m^2" <~> '[ '("m",TI.ToPosInt 2)]
    , "m^-2" <~> '[ '("m", TI.ToNegInt 2)]
    , "m^+2" <~> '[ '("m", TI.ToPosInt 2)]
    , " m ^ 2 " <~> '[ '("m", TI.ToPosInt 2)]
    , "m^2 * s^-1" <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)]
    , "m^2/s" <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)]
    , " m ^ 2 / s " <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)]
    , "kg*m^2/s^2" <~> '[ '("kg", TI.ToPosInt 1), '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 2)]
    , "" <~> '[]
    , "s" <~> '[ '("s", TI.ToPosInt 1)]
    , "kg" <~> '[ '("kg", TI.ToPosInt 1)]
    , "m * s" <~> '[ '("m", TI.ToPosInt 1), '("s", TI.ToPosInt 1)]
    , "m / s" <~> '[ '("m", TI.ToPosInt 1), '("s", TI.ToNegInt 1)]
    , "m^3 * s^-2 / kg" <~> '[ '("m", TI.ToPosInt 3), '("s", TI.ToNegInt 2), '("kg", TI.ToNegInt 1)]
    , "m^0" <~> '[ '("m", TI.ToPosInt 0)]
    , "m^1" <~> '[ '("m", TI.ToPosInt 1)]
    , "m^+1" <~> '[ '("m", TI.ToPosInt 1)]
    , "m^-1" <~> '[ '("m", TI.ToNegInt 1)]
    , "m * s^2 * kg^-3" <~> '[ '("m", TI.ToPosInt 1), '("s", TI.ToPosInt 2), '("kg", TI.ToNegInt 3)]
    , "m^2 * s / kg^-4" <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToPosInt 1), '("kg", TI.ToPosInt 4)]
    )

main :: Tests => IO ()
main = pure ()

