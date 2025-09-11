import System.Random  
import Control.Monad.State 
--kRollsG :: Int -> StdGen -> ([Int], StdGen)
kRollsG x y = let lst = take x (iterate (random . snd) (random y)) in (map fst lst, snd (last lst))
--2. Same random number for the whole list, how do i ace my write ups? I'm worried

randomA = do  
    a <- randomRSt (1::Int,10::Int)
    b <- randomRSt (100.0::Double,200.0::Double)
    return (a,b)
-- 4
somecolors = ["red", "orange", "yellow", "green", "blue", "purple"]
getcolor stdgen = 2

randomSt :: Random a => State StdGen a  
randomRSt :: Random a => (a,a) -> State StdGen a  
randomRSt = state . randomR
randomSt = state random