import Control.Monad (join)
import Control.Applicative (liftA2)
import Data.List.Extra
import Data.List.HT
import Data.Maybe (fromJust)
import Data.Bits (xor)

asclistdif alla@(a:as) allb@(b:bs) = case a `compare` b of
    GT -> asclistdif alla bs
    EQ -> asclistdif as bs
    LT -> a : asclistdif as allb
asclistdif a [] = a
asclistdif [] _ = []

firstrow a = a:[0..a-1]++[a+1..]
myiterate :: ([a] -> a) -> a -> [a]
myiterate fn base = base : go [base] where
    go accum = let ans = fn accum in ans : go (ans:accum)
shownum :: Int -> String
shownum a = shown ++ replicate (2- length shown) ' ' where
    shown = show a
shownum3 :: Int -> String
shownum3 a = shown ++ replicate (3- length shown) ' ' where
    shown = show a

display2lst :: [[Int]] -> String
display2lst = init . unlines . reverse . map (unwords . map shownum) . reverse . take 53 . map (take 53)
display2lst3 :: [[Int]] -> String
display2lst3 = init . unlines . map (unwords . map shownum3) . reverse . take 30 . map (take 30)

odisplay2lst = init . unlines . map (unwords . map shownum) . reverse . take 53 . map (take 53)
lst2make pastelems pastlsts = ans : lst2make (insert ans pastelems) (map tail pastlsts)
    where ans = head (asclistdif [0..] (merge pastelems (sort (map head pastlsts))))
make2lsts = putStrLn . display2lst . mk2lsts
makedif = writeFile "textfile3.txt" . display2lst3 .  mapAdjacent (zipWith subtract) . mk2lsts
filemake2lsts = writeFile "textile.txt" . odisplay2lst . mk2lsts
mk2lsts :: Int -> [[Int]]
mk2lsts = myiterate (lst2make []) . firstrow
the2lsts :: [[Int]]
the2lsts = mk2lsts (-1)
diff :: [[Int]] -> [[Int]]
diff a = (zipWith . zipWith) (-) (tail (tail <$> a)) a 
difflsts = putStrLn . display2lst3 . diff . mk2lsts 
good2lsts = (putStrLn . odisplay2lst)
  (map (( \ x_ -> zipWith xor x_ [0 .. ])) (map repeat [0 .. ]))

