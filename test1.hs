main = do 
    x <- readFile "input.txt"
    writeFile "synergism.txt" $ unlines $ map (head . words) $ lines x 