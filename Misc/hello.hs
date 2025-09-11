import Control.Lens 
import Control.Monad.Trans.State
import Data.Functor.Compose
import Data.Coerce(coerce)
myzoom :: LensLike' (Compose m ((,) a)) t b -> StateT b m a -> StateT t m a
myzoom = coerce
-- this is zoom