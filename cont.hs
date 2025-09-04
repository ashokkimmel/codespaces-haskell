{-# LANGUAGE DefaultSignatures #-}
import Control.Applicative (liftA2)
import Control.Monad (ap)
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Control.Monad.Trans.Except ( ExceptT(..) )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Control.Monad.Trans.Identity ( IdentityT(IdentityT) )
import Control.Monad.Trans.RWS ( RWST(..) )
import Control.Monad.Trans.Writer ( WriterT(WriterT) )
import Control.Monad.Trans.Accum ( runAccumT, AccumT(..) )
import Control.Monad.Trans.State ( StateT(..) )
import MonadBind ( lift2Monad, MonadBind ) 
import Text.Megaparsec ( ParsecT, Stream )
import Text.Megaparsec.Internal ( ParsecT(ParsecT) )
data Confirmation a = Final a | Confirm (Confirmation a) (Confirmation a)
    deriving (Show, Eq, Ord, Functor, Foldable)
instance Applicative Confirmation where
    pure = Final
    (Final a) <*> x = fmap a x 
    (Confirm a b) <*> z = Confirm (a <*> z) (b <*> z)
instance Monad Confirmation where
    (Final a) >>= x = x a
    (Confirm a b) >>= x = Confirm (a >>= x) (b >>= x)
class Monad m => MonadConfirm m where 
    confirm :: m a -> m a -> m a
    default confirm :: ((m ~ t x), MonadBind t,MonadConfirm x) => m a -> m a -> m a
    confirm = lift2Monad confirm 
instance MonadConfirm Confirmation where
    confirm :: Confirmation a -> Confirmation a -> Confirmation a
    confirm = Confirm

instance MonadConfirm m => MonadConfirm (MaybeT m)
-- ExceptT instance
instance MonadConfirm m => MonadConfirm (ExceptT e m)
-- ReaderT instance
instance MonadConfirm m => MonadConfirm (ReaderT r m)
-- IdentityT instance
instance MonadConfirm m => MonadConfirm (IdentityT m)
-- RWS instance
instance (Monoid w, MonadConfirm m) => MonadConfirm (RWST r w s m)
-- WriterT instance
instance (Monoid w, MonadConfirm m) => MonadConfirm (WriterT w m)
-- StateT instance
instance MonadConfirm m => MonadConfirm (StateT s m)
-- AccumT instance
instance (Monoid s, MonadConfirm m) => MonadConfirm (AccumT s m) 
-- ParsecT instance
instance (MonadConfirm m,Stream s) => MonadConfirm (ParsecT e s m) where
confirm (ParsecT p) (ParsecT q) = ParsecT $ \s cok cerr eok eerr -> confirm (p s cok cerr eok eerr) (q s cok cerr eok eerr)
