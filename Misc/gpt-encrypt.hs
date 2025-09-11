{-# LANGUAGE OverloadedLists #-}
import BasicDefs
myfun :: V (V2 (V8 E)) Int 
type Tbt= V (V2 E) (V (V2 E) Int) 
myfun = [1..100]
matrixmult :: (Functor f,IsVec a, Num b,IsVec c) => f (a b) -> (a (c b)) -> f (c b) 
matrixmult a b = 
    where vecstoint a = product . liftA2 (*) a 
    --[1,2,3]
    --[[1,2],[2,3],[3,4]]
    