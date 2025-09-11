import Data.List

badstrings = ["ab", "cd", "pq", "xy"]
badstringcontainer str = not (any (flip isInfixOf str) badstrings)
vowels = "aeiou"
vowel = (>= 3) . length . filter (flip elem vowels)
doubleinarow [_] = False 
doubleinarow (x:xs)
    |head xs == x =  True 
    |otherwise  = doubleinarow xs  
twopair [_] = False 
twopair xs 
    |isInfixOf (take 2 xs) (drop 2 xs) = True
    |otherwise = twopair (tail xs)  

repeatwittthoneinmiddle [_,_] = False 
repeatwittthoneinmiddle xs@(a:_:b:_) 
    |a == b = True
    |otherwise = repeatwittthoneinmiddle (tail xs)  

goodstr  x = and (map ($ x) [repeatwittthoneinmiddle, twopair])
howmanynice = length . filter goodstr . lines 
maketuple a b = (a,b)
main = do 
    x <- readFile "input.txt"
    let y = format x
    let (a,b) = move y (0,1)
    print (a+b)
format :: String -> [(Int, Char)]
format = (zip <$> map (read . init . tail) <*> map head) . words
rotate 'L' (a,b) = (-b,a)
rotate 'R' (a,b) = (b,-a)
data Line = HLine Int Int Int | VLine Int Int Int 
between x y z = (x <= y) && (y <= z) || (x >= y) && (y >= z)
lineintersect (HLine y x1 x2) (VLine x y1 y2) = between x1 x x2 && between y1 y y2 
lineintersect tv@(VLine _ _ _) th@(HLine _ _ _)= lineintersect th tv
lineintersect _ _ = False  

findJust [] = Nothing
findJust (Nothing:xs) = findJust xs 
findJust (x:_) = x
data Direction = NORTH | EAST | SOUTH | WEST deriving (Show, Enum, Ord,Eq)
rotatedir :: Char -> Direction -> Char 
rotatedir 'L' = pred
rotatedir 'R' = succ
moveline :: [(Int,Char)] -> (Int,Int) -> (Int,Int) -> (Int,Int)
moveline ((howfar,turn):rest) [Line] (oldx,oldy) Direction = 
move :: [(Int,Char)] -> (Int,Int) -> (Int,Int)
move [] _ = (0,0)
move ((x,n):xs) z = (a + nx * x, b + yx * x)
    where (nx,yx) = rotate n z 
          (a,b) = move xs (nx,yx) 