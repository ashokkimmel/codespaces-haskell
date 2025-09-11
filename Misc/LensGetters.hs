{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}

--module LensGetters where
import Control.Lens
import Data.Semigroup (Endo(..))
import Data.Coerce (coerce, Coercible)
(~^.) ::  (Profunctor p, Contravariant f) =>
     ASetter s t a b -> Getting b a b -> Optic' p f s t
-- (a -> b) -> s -> t
-- (a -> b) -> s -> t
(~^.) = dimap view to . over
infixr 9 ~^.
(~^..) ::  (Profunctor p, Contravariant f) =>
     ASetter s t a [b] -> Getting (Endo [b]) a b -> Optic' p f s t
(~^..) =   dimap toListOf to . over
infixr 9 ~^..

coerced' :: forall f a b. (forall a. Coercible a (f a)) => Iso a b (f a) (f b)
coerced' = coerced 