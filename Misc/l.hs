{-# LANGUAGE TypeFamilies,FunctionalDependencies,AllowAmbiguousTypes #-}
import Data.List as List 
import Data.Set as Set 
import Data.Map as Map
import Data.Maybe (fromJust) 
import Hoogle
class Lookup a b c where 
    type LookupType a b c
    lookup :: b -> LookupType a b c -> (Maybe c)
instance Eq a => Lookup [] a b where 
    type LookupType [] a b = [(a,b)]
    lookup = List.lookup
{- instance Ord a => Lookup Set.Set a b where 
    type LookupType Set.Set a b = Set.Set (a,b)
    lookup = -}
instance Lookup (->) a b where 
    type LookupType (->) a b = (a -> b)
    lookup a = Just . flip ($) a 
instance Ord a => Lookup Map.Map a b where 
    type LookupType Map.Map a b = Map.Map a b 
    lookup = Map.lookup
class Get a b c d | a b c -> d, d -> a,d->b,d->c where 
    getUnsafe :: d -> b -> c 
    --getUnsafe a = fromJust . get a 
    get :: d -> b -> Maybe c
    get a = Just . getUnsafe a 
instance Get [] Int a [a] where 
    getUnsafe = (!!)
instance Ord a => Get Map.Map a b (Map.Map a b) where 
    getUnsafe = (Map.!)
type GetType a b = 
