module MyLenses where 
import Data
import Control.Lens hiding (Control.Lens.Tutorial)
import Control.Applicative (liftA3)
pl1 :: Lens' Board Single
pl1 = makeLens' (toSingle True) (flip (setBoard True))
pl2 :: Lens' Board Single
pl2 = makeLens' (toSingle False) (flip (setBoard False))
toSingle True  = liftA2 Single _pl1Atk _pl2Ship <&> True  
toSingle False = liftA2 Single _pl2Atk _pl1Ship <&> False 
setBoard True  single = set turn (boolToPlayer . view whoseturn $ single). set pl1Atk (view attacks single) . set pl2Ship (view eships single)
setBoard False single = set turn (boolToPlayer . view whoseturn $ single). set pl2Atk (view attacks single) . set pl1Ship (view eships single)
boolToPlayer :: Bool -> Player
boolToPlayer True = Pl1
boolToPlayer False = Pl2
