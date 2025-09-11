{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Lens
import System.Random 
import Control.Monad (join)
import qualified Data.Vector as V 
import qualified Data.Map as M
import qualified Data.Set as S 
import Data.Set (Set) 
import Data.Map (Map) 
import Data.Bifunctor
import Data.Vector (Vector)
--import GHC.IO (unsafeperformIO)
type instance Index (Map a b) = a                                                                                                                                                                                                      
type instance IxValue (Map a b) = b
instance Ord a => Ixed (Map a b) where 
    ix a = \func val -> maybe (pure val) (fmap (M.singleton a)) (fmap func (M.lookup a val))
instance Ord a => At (Map a b)  where 
    at a = lens (M.lookup a) (flip (\s -> (M.alter (const s) a)))
type instance Index (Set a) = a 
type instance IxValue (Set a) = ()
instance Ord a => Ixed (Set a) 
instance Ord a => At (Set a) where 
    at a = lens (\b -> ifThenElse (elem a b) (Just ()) Nothing) (flip (($ a) . maybe S.delete (const S.insert))) 
ifThenElse a b c = if a then b else c 

memoryamount = 131072
startmemory :: Vector Bool 
startmemory = V.fromListN memoryamount (randoms $ mkStdGen 0) -- 128 KB -- Kinda sad


data OS = OS { 
    _memory    :: Vector Bool,
    _variables :: Map String Int,
    _printOut :: String,
    _usedUp :: Set (Int,Int)} deriving (Show,Eq) -- Eq might be a mistake,you shouldn't need it 

makeLenses 'OS 
startOs = OS startmemory [] []
findMemory :: [(Int,Int)] -> Int -> Int
findMemory (t1@(_,b):rest@((c,_):z)) n
    | b - c <= n = b
    | otherwise = findMemory rest n
findMemory [(_,b)] n = b 
firstFindMemory whole@(other@(a,_):z) n  
    | a <= n = 0
    | otherwise = findMemory z n 
allocate :: Int -> String -> OS -> OS
allocate bytes name oldos = oldos  & variables . at name ?~ newptr & (usedUp. at (newptr,newptr +bytes)) ?~ () where 
    allocatedmemory = oldos ^.. usedUp . folded
    newptr = firstFindMemory allocatedmemory bytes 
    allocatedmemory :: [(Int,Int)]
viewset = join (,)
-- 

stupidtraverse :: Traversal' a b
stupidtraverse = const pure