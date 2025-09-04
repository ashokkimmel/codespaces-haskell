-- import Control.Monad.RWS (MonadState(put))
-- import System.Posix.Internals (puts)
-- import Control.Monad.Reader
-- main = do 
--     a <- getLine
--     putStrLn (a++a)
--     putStrLn "Done"
-- main2 =  getLine >>= \a -> putStrLn (a++a) >> putStrLn "Done"
-- other :: ReaderT String IO ()
-- other = do
--     a <- myask
--     lift $ putStrLn (a ++ a)
--     lift $ putStrLn "Done"
-- myask :: Applicative f => ReaderT a f a 
-- myask = ReaderT pure
-- mjoin :: Monad m => m (m a) -> m a
-- mjoin = (>>= id)
-- bind :: Monad m => (a -> m b) -> m a -> m b
-- bind f m = mjoin (fmap f m)
-- sillyfmap :: Functor m => (a -> b) -> ReaderT r m a -> ReaderT r m b
-- sillyfmap f (ReaderT g) = ReaderT (fmap f . g)
-- sillyjoin :: Monad m => ReaderT r m (ReaderT r m a) -> ReaderT r m a
-- sillyjoin (ReaderT g) = ReaderT $ \r -> mjoin (fmap (($ r). runReaderT) (g r))
-- -- g :: r -> m (ReaderT r m a)
-- -- g r :: m (ReaderT r m a)
-- -- fmap runReaderT (g r) :: m (r -> m a)
-- -- fmap (($ r). runReaderT) (g r) :: m (m a)
-- -- mjoin (fmap (($ r). runReaderT) (g r)) :: m a
-- 
-- fun :: Int -> String 
-- fun n 
--     | n `mod` 5 == 0 = show n ++ replicate (5 - length (show n)) ' '
--     | otherwise      = replicate 7 ' '
-- fun2 :: (Int,String) -> String
-- fun2 (n,s) = fun n ++ s
-- write :: IO ()
-- write = do
--     text <- readFile "textile.txt"
--     let semilinedtxt = zip [1..] (lines text)
--     
--     writeFile "linedfile.txt" (unlines (map fun2 semilinedtxt))
-- 
{-#LANGUAGE ScopedTypeVariables, KindSignatures, GADTs, ConstraintKinds, FunctionalDependencies, RankNTypes#-}
{-#LANGUAGE AllowAmbiguousTypes#-}

import Unsafe.Coerce (unsafeCoerce)

import Data.Kind (Constraint, Type)
import Data.Coerce (Coercible)

data Dict (c :: Constraint) where
    Dict :: c => Dict c

withLocal :: forall c a b d. (c a, Coercible a b) => (c b => d) -> d
withLocal x = case unsafeCoerce (Dict :: Dict (c a)) :: Dict (c b) of Dict -> x

data FakeDict d = FakeDict d

class FakeDictFor (c :: Constraint) (d :: Type) | d -> c

withFakeDictUnsafe :: forall c d a. d -> (c => a) -> a
withFakeDictUnsafe d x = case unsafeCoerce (FakeDict d) :: Dict c of
    Dict -> x

withFakeDict :: forall d c a. FakeDictFor c d => d -> (c => a) -> a
withFakeDict = withFakeDictUnsafe

