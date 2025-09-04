{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeNats
import Data.List (intercalate)
import Control.Monad.Zip
import Control.Applicative (liftA2)
import Types (ToPeano, Zero, Succ)
class MapN num a b c d | num a -> c , num b -> d, num a d -> b, num b c -> d where
    mapN :: (c -> d) -> a -> b
instance MapN Zero a b a b where
    mapN = id
    {-# INLINE mapN #-}
instance (Functor g, MapN x a b (g e) (g f)) => MapN (Succ x) a b e f where
    mapN = mapN @x . fmap
    {-# INLINE mapN #-}
mapn :: forall n a b c d. (MapN (ToPeano n) a b c d) => (c -> d) -> a -> b
mapn = mapN @(ToPeano n)
{-# INLINE mapn #-}
class Applicative f => LiftN' a f c d | a d c -> f, a f c -> d  where
    liftN' :: c -> d
class Applicative f => LiftN a f c d | a d c -> f, a f c -> d  where
    liftN :: c -> d
instance Applicative f => LiftN Zero f a (f a) where
    liftN = pure
    {-# INLINE liftN #-}
instance Applicative f => LiftN (Succ Zero) f (a->b) (f a-> f b) where
    liftN = fmap
    {-# INLINE liftN #-}
instance (LiftN' a b c d) => LiftN (Succ (Succ a)) b c d where liftN = liftN' @a @b @c @d 
instance Applicative f => LiftN' Zero f (a -> b -> c) (f a -> f b -> f c) where
    liftN' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    liftN' = liftA2 
    {-# INLINE liftN' #-}
instance (Applicative f, LiftN' x f y z, MapN x z m (f (a -> b)) (f a -> f b)) => LiftN' (Succ x) f y m where
    liftN' = mapN @x (<*>) . liftN' @x @f @y @z
    {-# INLINE liftN' #-}

liftAn :: forall n f start end. (Applicative f, LiftN (ToPeano n) f start end) => start -> end
liftAn = liftN @(ToPeano n)  -- . (pure @f)
{-# INLINE liftAn #-}
class ListN num a where
    listNp :: a
instance ListN Zero [a] where
    listNp = []
instance (ListN x xs,MapN x xs y [a] [a]) => ListN (Succ x) (a -> y) where
    listNp x = mapN @x @xs (x:) (listNp @x @xs)
list :: forall n a. (ListN (ToPeano n) a) => a
list = listNp @(ToPeano n) @a

-- This will give odd error messages, so be warned
class MonadZip f=> MonadZipn num f a b | num f a -> b where
    mZipWithnp :: (a -> b)
instance MonadZip f => MonadZipn (Succ (Succ Zero)) f (a -> b -> c) (f a -> f b -> f c) where
    mZipWithnp = mzipWith
instance (MonadZip f, MonadZipn x f y z, MapN x z m (f (a -> b)) (f a -> f b)) => MonadZipn (Succ x) f y m where
    mZipWithnp = mapN @x (mzipWith id) . mZipWithnp @x @f @y @z

mzipWithn :: forall n f a b c. (MonadZipn (ToPeano n) f a b) => a -> b
mzipWithn = mZipWithnp @(ToPeano n) @f
zipWithn :: forall n a b c. (MonadZipn (ToPeano n) [] a b) => a -> b 
zipWithn = mZipWithnp @(ToPeano n) @[]


-- class LiftAmn m n a b where
--     liftAmnp :: a -> b
-- instance LiftAmn Zero x a a where
--     liftAmnp = id
-- instance (LiftAmn x n a b, Applicative f, liftN' n f (f b) c) => LiftAmn (Succ x) n a c where
--     liftAmnp = liftN' @n @f . liftAmnp @x
-- This was an attempt to create a class that would be a combination of MapN and LiftN', but it didn't work out as it is impossible to satisfy the fundep.
-- myfun :: Int -> String 
-- myfun n = str11 ++ nl ++ str21 ++ nl ++ str31 ++ inlineM ++ "\n\n" ++ str12
--     where 
--         charlist = map (('v':).show) [1..]
--         nl = "\n    "
--         tuples = '(' : intercalate ", " (take n charlist) ++ ")"
--         succs = concatreplicate n "(Succ " ++ "Zero" ++ replicate n ')'
--         variables = unwords (take n charlist)
--         inlineM = nl ++ "{-# INLINE curryM #-}" ++ nl ++ "{-# INLINE uncurryM #-}"
--         classstring = " (" ++ intercalate " -> " (take (n+1) charlist) ++ ") ("++ tuples ++ " -> " ++ (charlist !! n)++")"
--         str11 = "instance CurryingM " ++ classstring ++ " where"
--         str21 = "curryM z " ++ variables ++ " = z " ++ tuples
--         str31 = "uncurryM z " ++ tuples ++ " = z " ++ variables
--         str12 = "instance CurryingN  " ++ succs ++ classstring
--         str22 = "uncurryNp z " ++ tuples ++ " = z " ++ variables
--         str32 = "curryNp z " ++ variables ++ " = z " ++ tuples
-- concatreplicate :: Int -> [a] -> [a]
-- concatreplicate n xs = concat (replicate n xs) 
-- main = do 
--     x <- readFile "template.hs"
--     writeFile "c.hs" $ x ++ intercalate "\n\n" (map myfun [3..64])
-- final = readFile "c.hs" >>= writeFile "Currying.hs"
{- TODO
Change code/make better
Download the file/test outside of codespaces
Create tests
Dicide license, MIT?
Add haddock documentation -} 