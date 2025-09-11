import Data.List (permutations, nub)
import Data.Ratio
import qualified Data.Set as Set

-- Operations
data Op = Add | Sub | Mul | Div  deriving (Eq, Ord)

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- Expression
data Expr = Val Rational | App Op Expr Expr deriving (Eq, Ord)

instance Show Expr where
    show (Val n) = showRational n
    show (App op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

showRational :: Rational -> String
showRational r
    | denominator r == 1 = show (numerator r)
    | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

-- Evaluate an expression safely
eval :: Expr -> Maybe Rational
eval (Val n) = Just n
eval (App op l r) = do
    x <- eval l
    y <- eval r
    case op of
        Add -> Just (x + y)
        Sub -> Just (x - y)
        Mul -> Just (x * y)
        Div -> if y /= 0 then Just (x / y) else Nothing

-- Check if operation is commutative
isCommutative :: Op -> Bool
isCommutative Add = True
isCommutative Mul = True
isCommutative _   = False

-- Generate all possible expressions
exprs :: [Rational] -> Set.Set Expr
exprs [] = Set.empty
exprs [n] = Set.singleton (Val n)
exprs ns = Set.fromList
    [App op l r |
        (ls, rs) <- split ns,
        l <- Set.toList (exprs ls),
        r <- Set.toList (exprs rs),
        op <- ops,
        isValid op l r
    ]

-- Only generate valid operations (no duplicates for commutative ops)
isValid :: Op -> Expr -> Expr -> Bool
isValid op l r
    | isCommutative op && l > r = False -- avoid symmetric duplicates
    | otherwise = case op of
        Div -> case eval r of
                  Just 0 -> False
                  _      -> True
        _   -> True

-- All operations to consider
ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- Split list into all non-empty pairs
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- Solve for target
solve24 :: [Integer] -> [String]
solve24 = solven 24 
solven :: Integer -> [Integer] -> [String]
solven n nums = nub
    [show e |
        ns <- permutations nums,
        let rationals = map (% 1) ns,
        e <- Set.toList (exprs rationals),
        eval e == Just (n % 1) ]
-- Main program
main :: IO ()
main = do
    putStrLn "Enter 4 integers separated by space:"
    input <- getLine
    let nums = map read (words input) :: [Integer]
    if length nums /= 4
        then putStrLn "Please enter exactly 4 numbers."
        else do
            let solutions = solve24 nums
            if null solutions
                then putStrLn "No solution found."
                else do
                    putStrLn $ "Solutions found: " ++ show (length solutions)
                    mapM_ putStrLn solutions
