{-# LANGUAGE ViewPatterns#-}
module Update where
import Data.List (null,delete)
import Data 
import MyLenses
import qualified Data.Set as Set hiding (Set) 
import Data.Set (Set)
import Data.Semigroup (Any(..),All(..))
import Control.Arrow ((&&&))
import Control.Lens hiding (Control.Lens.Tutorial)
fullupdate :: (Int,Int) -> Board -> Board 
fullupdate = (gamend.) . update
anychange :: Board -> Board -> Bool 
anychange x y = not . getAll $ g pl1Ship <> g pl2Ship where
    g func = All $ view func x == view func y

update :: (Int,Int) -> Board -> Board 
update oldloc@(a,b) oldboard@(view turn -> None) = oldboard
update oldloc@(a,b) oldboard@(view turn -> Pl1) = over pl1 (updatesingle oldloc) oldboard
update oldloc@(a,b) oldboard@(view turn -> Pl2) = over pl2 (updatesingle oldloc) oldboard
gamend :: Board -> Board 
gamend fn@(whowon -> None) = fn
gamend fn@(whowon -> x) = set winner x . set turn None $ fn

whowon :: Board -> Player
whowon oldboard
    | all shipdead $ _pl1Ship oldboard = Pl2
    | all shipdead $ _pl2Ship oldboard = Pl1
    | otherwise = None 
shipdead :: Ship -> Bool
shipdead = null . view partsLeft
attackmade :: (Int,Int) -> Single -> Bool
attackmade a = getAny . (Any . Set.member (uncurry Miss a) . _attacks <> Any . Set.member (uncurry Hit a) . _attacks)
hitship :: (Int,Int) -> Single -> Bool 
hitship x y = any (Set.member x) (toListOf (eships.traverse. partsLeft) y)
hasattack :: (Int,Int) -> Set Attack -> Bool 
hasattack = (getAny.) . (toAny Miss <> toAny Hit) 
    where toAny :: (Int -> Int -> Attack) -> (Int,Int)-> Set Attack -> Any
          toAny fn = (Any .) . Set.member. uncurry fn

updatesingle :: (Int,Int)-> Single -> Single
updatesingle x y
    | attackmade x y = y
    | hitship x y = over whoseturn not . over attacks (Set.insert (uncurry Hit x)) . over (eships.traverse. partsLeft) (Set.delete x) $ y
    | otherwise = over whoseturn not . over attacks (Set.insert (uncurry Miss x)) $ y
addship :: (Int,Int) -> Int -> Board -> (Maybe Board)
addship x y = Nothing

check :: (a -> Bool) -> a -> Maybe a 
check func = ($>) <*> (guard . func)