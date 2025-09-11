getXofN :: Int -> [a] -> [[a]]
getXofN 0 _ = []
getXofN 1 x = map (:[]) x
getXofN n lst@(x:xs) 
    | n >= length lst =[lst]  
    | otherwise = getXofN n xs ++ map (x:) (getXofN (n-1) xs)
