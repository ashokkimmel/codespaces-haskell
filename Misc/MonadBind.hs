{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,QuantifiedConstraints,DefaultSignatures,AllowAmbiguousTypes,TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wall #-}
module MonadBind where
import Data.Coerce ( coerce, Coercible )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )
import Control.Monad.Trans.Except ( ExceptT(..) )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Control.Monad.Trans.Identity ( IdentityT(IdentityT) )
import Control.Monad.Trans.RWS ( RWST(..) )
import Control.Monad.Trans.Writer ( WriterT(WriterT) )
import Control.Monad.Trans.Accum ( runAccumT, AccumT(..) )
import Control.Monad.Trans.State ( StateT(..) )
import Control.Monad.Trans ( MonadTrans )
-- import Control.Monad
import Data.Kind (Type)
class (forall m. Monad m => Monad (t m), MonadTrans t) => MonadBind t where
    type Inside t c :: Type
    mbind :: (m (Inside t a) -> t m b) -> t m a -> t m b
    mpure :: m (Inside t a) -> t m a
    default mbind :: (Coercible (t m a) (m (Inside t a)), Coercible (t m b) (m (Inside t b))) =>  (m (Inside t a) -> t m b) -> t m a -> t m b
    mbind = coerce
    default mpure :: (Coercible (m (Inside t a)) (t m a)) => m (Inside t a) -> t m a
    mpure = coerce
lift2Monad :: (MonadBind t,Monad m) => (m (Inside t a) -> m (Inside t b) -> m (Inside t c)) -> t m a -> t m b -> t m c
lift2Monad f x y = (mbind . (.) ($ y) . (.) mbind . ((.) . (.)) mpure) f x
lift1Monad :: (MonadBind t, Monad m) => (m (Inside t a) -> m (Inside t b)) -> t m a -> t m b
lift1Monad = mbind . (.) mpure


-- MaybeT instance
instance MonadBind MaybeT where
    type Inside MaybeT a = Maybe a

-- ExceptT instance
instance MonadBind (ExceptT e) where
    type Inside (ExceptT e) a = Either e a
-- 
-- ReaderT instance
instance MonadBind (ReaderT r) where
    type Inside (ReaderT r) a = a
    mpure = ReaderT . const
    mbind f (ReaderT g) = ReaderT $ \r -> runReaderT (f (g r)) r

-- IdentityT instance
instance MonadBind IdentityT where
    type Inside IdentityT a = a

-- RWS instance
instance Monoid w => MonadBind (RWST r w s) where
    type Inside (RWST r w s) a = (a,s,w)
    mpure = RWST  . const . const
    mbind f (RWST x) = RWST $ \r s -> let tuple' = f (x r s) in runRWST tuple' r s
-- WriterT instance
instance Monoid w => MonadBind (WriterT w) where
    type Inside (WriterT w) a = (a,w)

-- StateT instance
instance MonadBind (StateT s) where
    type Inside (StateT s) a = (a,s)
    mpure = StateT . const
    mbind f (StateT x) = StateT $ \s -> let m = x s in runStateT (f m) s

-- AccumT instance 
instance Monoid s => MonadBind (AccumT s) where
    type Inside (AccumT s) a = (a,s)
    mpure = AccumT . const
    mbind f (AccumT x) = AccumT $ \s -> let m = x s in runAccumT (f m) s