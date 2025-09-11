import Data.Bifunctor
-- | Compute the factorial of a number
-- factorial :: Int -> Int

-- | Compute the factorial of a number, in continuation passing style
cpsFactorial:: Int -> (Int -> r) -> r
cpsFactorial n k
    | n <= 0    = k 1
    | otherwise = cpsFactorial (n-1) (k.(*n))

-- | Compute the n-th fibonacci number F(n).
--    Recall F(0) = 0, F(1) = 1, and F(n) = F(n-1) + F(n-2)
-- fibonacci :: Int -> Int

-- | Compute the n-th fibonacci number F(n), in continuation passing style
cpsFibonacci:: Int -> (Int -> r) -> r
cpsFibonacci 0 k = k 0
cpsFibonacci 1 k = k 1 
cpsFibonacci n k = cpsFibonacci (n-2) $ \m -> cpsFibonacci (n-1) (k . (+m))


-- | Sample tests:
prop_testFactorial :: Bool
prop_testFactorial = (cpsFactorial 3 id) == 6
prop_testFibonacci :: Bool
prop_testFibonacci = (cpsFibonacci 6 id) == 8

------------------------------------------------------------------------------
-- | List functions

-- | CPS transform of the function `length`, which computes the length of a list
cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = k 0
cpsLength (_:a) k = cpsLength a (k . succ)

-- | CPS transform of the function `map`. The argument function (to be applied
--   every element of the list) is written in direct style
cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = k []
cpsMap f (a:b) k = cpsMap f b (k . (f a:)) 


-- | Sample tests:
prop_cpsLength :: Bool
prop_cpsLength = (cpsLength [1,2,3] id) == 3
prop_cpsMap :: Bool
prop_cpsMap = (cpsMap (2*) [1,2,3,4,5] id) == [2,4,6,8,10]

------------------------------------------------------------------------------
-- Merge Sort

-- | Sort a list using mergeSort
-- mergeSort :: [Int] -> [Int]

-- | Split a list into two lists. All list elements in even indices
-- is placed in one sub-list, and all list elements in odd indicies
-- is placed in the second sub-list.
-- split :: [Int] -> ([Int], [Int])

-- | Merge two sorted lists together
-- merge :: [Int] -> [Int] -> [Int]

-- | CPS transform of mergeSort
cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort lst k = undefined

-- | CPS transform of split
cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit lst k = cpsSplitn (length lst `div` 2) lst k

cpsSplitn :: Int -> [Int] -> (([Int],[Int]) -> r) -> r
cpsSplitn 0 x k = k ([],x)
cpsSplitn n (a:b) k = cpsSplitn (n-1) b (k.first (a:))

-- | CPS transform of merge
cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge [] a k = k a 
cpsMerge a [] k = k a 
cpsMerge (a:b) (c:d) k = case compare a c of 
    LT -> cpsMerge b (c:d)  (k.(a:))
    EQ -> cpsMerge b d (k.(a:).(c:))
    GT -> cpsMerge (a:b) d  (k.(c:))

-- | Sample test:
prop_cpsMergeSort :: Bool
prop_cpsMergeSort = (cpsMergeSort [1,2,4,3] id) == [1,2,3,4]