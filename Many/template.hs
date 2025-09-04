{-# LANGUAGE FunctionalDependencies, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes,TypeApplications,ScopedTypeVariables #-}

import Types 
class CurryingM a b | b -> a where
    curryM :: b -> a
    uncurryM :: a -> b
class CurryingM a b => CurryingN num a b | num a -> b, b -> a where
    curryNp :: b -> a
    curryNp = curryM @a @b 
    uncurryNp :: a -> b
    uncurryNp = uncurryM @a @b
    {-# INLINE curryNp #-}
    {-# INLINE uncurryNp #-}
makeTuple :: forall num a1 a2. CurryingN (ToPeano num) a1 (a2 -> a2) => a1 
makeTuple = curryNp @(ToPeano num) id
curryN :: forall num a1 a2. CurryingN (ToPeano num) a1 a2 => a2 -> a1
curryN = curryNp @(ToPeano num)
uncurryN :: forall num a1 a2. CurryingN (ToPeano num) a1 a2 => a1 -> a2
uncurryN = uncurryNp @(ToPeano num)

instance CurryingM v1 (() -> v1) where
    curryM z  = z ()
    uncurryM z _ = z 

instance CurryingN Zero v1 (() -> v1) where

instance CurryingM  (v1 -> v2 -> v3) ((v1, v2) -> v3) where
    curryM = curry 
    uncurryM = uncurry  

instance CurryingN (Succ (Succ Zero)) (v1 -> v2 -> v3) ((v1, v2) -> v3) 
