ex1 = matrixEncrypt [4,30,80,90] [[1,6],[4,8]]
matrixEncrypt :: [Int] -> [[Int]] -> [Int]
matrixEncrypt msg finitekey = 
    let key =  take (length msg) (cycle finitekey)
    in [sum [a * b | (a, b) <- zip (drop (counter * length key) msg) row]| 
        (row,counter) <- zip key [0..length finitekey]]
fit :: String -> String