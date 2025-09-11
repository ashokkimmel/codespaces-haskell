{-# LANGUAGE StandaloneKindSignatures,FunctionalDependencies,TypeFamilies,AllowAmbiguousTypes,DataKinds,ScopedTypeVariables #-}
import Data.Kind (Type, Constraint)
import Data.Functor.Const (Const(..))
import Control.Monad.Trans.Identity (IdentityT(..))
type AllMonad :: k -> ((k ->  Type) -> Constraint) -> ((k -> Type) -> k -> Type) -> Constraint
class AllMonad  (a :: k) c m | c -> a where
  pureAll :: c b => forall x. b x -> m b x
  bindAll :: c b => forall x. m (m b) x -> m b x
class Succeed a 
instance Succeed a 
instance AllMonad k Succeed IdentityT where 
    pureAll = IdententityT 
    bindAll (IdentityT x) = x
type Get :: (() -> Type) -> Constraint
class Get a where
    type Internal a :: *
    get :: a b -> Internal a
    put :: Internal a -> a b 
instance Get (Const a ::  () -> Type) where
    type Internal (Const a) = a
    get (Const x) = x
    put = Const
newtype LiftMonad m b a = LiftMonad { runLiftMonad :: forall x. m (b x) }
runLiftMonad' :: forall b m a. (Get b,Functor m) => LiftMonad m b a -> m (Internal b)
runLiftMonad' (LiftMonad x) = fmap (get @b) x
liftMonad' :: forall b m a. (Get b,Functor m) => m (Internal b) -> LiftMonad m b a
liftMonad' x = LiftMonad (fmap (put @b) x)
instance Monad m => AllMonad '() Get (LiftMonad m) where
    pureAll :: forall b. (Get b) => forall x. b x -> LiftMonad m b x
    pureAll a = LiftMonad (pure ((put @b . get @b) a)) 
    bindAll :: forall b. Get b => forall x. LiftMonad m (LiftMonad m b) x -> LiftMonad m b x
    bindAll mx = liftMonad' @b (runLiftMonad mx >>= runLiftMonad' :: (m (Internal b)))

c :: p1 -> p2 -> p1
c a _ =a