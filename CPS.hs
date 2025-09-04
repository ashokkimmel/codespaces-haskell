{-# LANGUAGE BlockArguments,TypeFamilies,QuasiQuotes#-}
import Control.Monad.Cont
import Data.String.Interpolate (i)
import Data.Char
import Debug.Trace
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Show
import Data.Functor.Classes
import Data.Functor.Foldable
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)
-- Generated through template-haskell
data ExprF r_aiQ2
  = NumF Int |
    VarF String |
    AddF r_aiQ2 r_aiQ2 |
    SubF r_aiQ2 r_aiQ2 |
    MulF r_aiQ2 r_aiQ2 |
    DivF r_aiQ2 r_aiQ2
  deriving (Functor, Foldable, Traversable)
type instance Base Expr = ExprF
instance Recursive Expr where
  project (Num x_aiQ3) = NumF x_aiQ3
  project (Var x_aiQ4) = VarF x_aiQ4
  project (Add x_aiQ5 x_aiQ6) = AddF x_aiQ5 x_aiQ6
  project (Sub x_aiQ7 x_aiQ8) = SubF x_aiQ7 x_aiQ8
  project (Mul x_aiQ9 x_aiQa) = MulF x_aiQ9 x_aiQa
  project (Div x_aiQb x_aiQc) = DivF x_aiQb x_aiQc
instance Corecursive Expr where
  embed (NumF x_aiQd) = Num x_aiQd
  embed (VarF x_aiQe) = Var x_aiQe
  embed (AddF x_aiQf x_aiQg) = Add x_aiQf x_aiQg
  embed (SubF x_aiQh x_aiQi) = Sub x_aiQh x_aiQi
  embed (MulF x_aiQj x_aiQk) = Mul x_aiQj x_aiQk
  embed (DivF x_aiQl x_aiQm) = Div x_aiQl x_aiQm
exprToNum :: Map String Int -> Expr -> Int
exprToNum env = cata alg
  where
    alg :: ExprF Int -> Int
    alg (NumF n)       = n
    alg (VarF v)       = case Map.lookup v env of
                          Just val -> val
                          Nothing  -> error $ "Undefined variable: " ++ v
    alg (AddF x y)     = x + y
    alg (SubF x y)     = x - y
    alg (MulF x y)     = x * y
    alg (DivF x y)     = x `div` y
printExpr :: Expr -> String
printExpr = cata alg
  where
    alg :: ExprF String -> String
    alg (NumF n)       = "(NumF " ++ show n ++ ")"
    alg (VarF v)       = "(VarF " ++ show v ++ ")"
    alg (AddF x y)     = "(AddF \n" ++ addIndent x ++ "\n" ++ addIndent y ++ ")"
    alg (SubF x y)     = "(SubF \n" ++ addIndent x ++ "\n" ++ addIndent y ++ ")"
    alg (MulF x y)     = "(MulF \n" ++ addIndent x ++ "\n" ++ addIndent y ++ ")"
    alg (DivF x y)     = "(DivF \n" ++ addIndent x ++ "\n" ++ addIndent y ++ ")"
addIndent :: String -> String
addIndent a = "  " ++ addIndent' a
addIndent' [] = []
addIndent' ('\n':xs) = "\n  " ++ addIndent' xs
addIndent' (x:xs) = x : addIndent' xs
data Token
  = TPlus | TMinus | TMul | TDiv
  | TLParen | TRParen
  | TNum Int
  | TVar String
  deriving (Show, Eq)

instance Show1 ExprF where
  liftShowsPrec a b c d =
                 case d of
                    NumF arg_achQ
                      -> showParen (c > 10)
                           (showString "NumF " . showsPrec 11 arg_achQ)
                    VarF arg_achR
                      -> showParen (c > 10)
                           (showString "VarF " . showsPrec 11 arg_achR)
                    AddF arg1_achS arg2_achT
                      -> showParen (c > 10)
                           (showString "AddF "
                              . a 11 arg1_achS
                                   . showSpace . a 11 arg2_achT)
                    SubF arg1_achU arg2_achV
                      -> showParen (c > 10)
                           (showString "SubF "
                              . a 11 arg1_achU
                                   . showSpace . a 11 arg2_achV)
                    MulF arg1_achW arg2_achX
                      -> showParen (c > 10)
                           (showString "MulF "
                              . a 11 arg1_achW
                                   . showSpace . a 11 arg2_achX)
                    DivF arg1_achY arg2_achZ
                      -> showParen (c > 10)
                           (showString "DivF "
                              . a 11 arg1_achY
                                   . showSpace . a 11 arg2_achZ)


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (ds, rest) = span isDigit (c:cs)
      in TNum (read ds) : tokenize rest
  | isAlpha c =
      let (vs, rest) = span isAlpha (c:cs)
      in TVar vs : tokenize rest
  | otherwise = case c of
      '+' -> TPlus   : tokenize cs
      '-' -> TMinus  : tokenize cs
      '*' -> TMul    : tokenize cs
      '/' -> TDiv    : tokenize cs
      '(' -> TLParen : tokenize cs
      ')' -> TRParen : tokenize cs
      _   -> error $ "Unknown character: " ++ [c]
type Parser = [Token] -> Cont (Either String Expr) (Expr, [Token])

-- === Expression Parser (handles + and -) ===
newparseExpr :: Parser
newparseExpr tokens = do
  (lhs, tokens') <- newparseTerm tokens
  trace [i|"NewParseExpr ran, lhs was #{lhs}"|] $ newparseExpr' lhs tokens'

newparseExpr' :: Expr -> [Token] -> Cont (Either String Expr) (Expr, [Token])
newparseExpr' lhs (TPlus : ts) = do
  (rhs, rest) <- newparseTerm ts
  newparseExpr' (Add lhs rhs) rest
newparseExpr' lhs (TMinus : ts) = do
  (rhs, rest) <- newparseTerm ts
  newparseExpr' (Sub lhs rhs) rest
newparseExpr' lhs ts = return (lhs, ts)

-- === Term Parser (handles * and /) ===
newparseTerm :: Parser
newparseTerm tokens = do
  (lhs, tokens') <- newparseFactor tokens
  newparseTerm' lhs tokens'

newparseTerm' :: Expr -> Parser
newparseTerm' lhs (TMul : ts) = do
  (rhs, rest) <- newparseFactor ts
  newparseTerm' (Mul lhs rhs) rest
newparseTerm' lhs (TDiv : ts) = do
  (rhs, rest) <- newparseFactor ts
  newparseTerm' (Div lhs rhs) rest
newparseTerm' lhs ts = return (lhs, ts)

-- === Factor Parser (numbers, variables, parentheses) ===
newparseFactor :: Parser
newparseFactor (TNum n : ts) = return (Num n, ts)
newparseFactor (TVar v : ts) = return (Var v, ts)
newparseFactor (TLParen : ts) = do
  (e, rest) <- newparseExpr ts
  case rest of
    (TRParen : ts') -> return (e, ts')
    _ -> cont $ \_ -> Left "Expected closing parenthesis"
newparseFactor _ = cont $ \_ -> Left "Unexpected token in factor"

-- === Top-level Interface ===
newrunParser :: String -> Either String Expr
newrunParser input =
  let tokens = tokenize input
  in runCont (newparseExpr tokens) checkRemaining
  where
    checkRemaining (expr, []) = Right expr
    checkRemaining (_, leftover) = Left $ "Unconsumed input: " ++ show leftover


-- === CPS Oldparser Type ===
type Oldparser r = [Token] -> (Expr -> [Token] -> r) -> r -> r

-- === Expression Oldparser (handles + and -) ===
parseExprCPS :: Oldparser r
parseExprCPS tokens succ fail =
  parseTermCPS tokens
    (\lhs tokens' -> parseExpr' lhs tokens' succ fail)
    fail
parseExpr' :: Expr -> Oldparser r
parseExpr' lhs (TPlus : ts) succ fail =
  parseTermCPS ts
    (\rhs rest -> parseExpr' (Add lhs rhs) rest succ fail)
    fail
parseExpr' lhs (TMinus : ts) succ fail =
  parseTermCPS ts
    (\rhs rest -> parseExpr' (Sub lhs rhs) rest succ fail)
    fail
parseExpr' lhs ts succ _ = succ lhs ts

-- === Term Oldparser (handles * and /) ===
parseTermCPS :: Oldparser r
parseTermCPS tokens succ fail =
  parseFactorCPS tokens
    (\lhs tokens' -> parseTerm' lhs tokens' succ fail)
    fail

parseTerm' :: Expr -> Oldparser r
parseTerm' lhs (TMul : ts) succ fail =
  parseFactorCPS ts
    (\rhs rest -> parseTerm' (Mul lhs rhs) rest succ fail)
    fail
parseTerm' lhs (TDiv : ts) succ fail =
  parseFactorCPS ts
    (\rhs rest -> parseTerm' (Div lhs rhs) rest succ fail)
    fail
parseTerm' lhs ts succ _ = succ lhs ts

-- === Factor Oldparser (numbers, variables, parentheses) ===
parseFactorCPS :: Oldparser r
parseFactorCPS (TNum n : ts) succ _ = succ (Num n) ts
parseFactorCPS (TVar v : ts) succ _ = succ (Var v) ts
parseFactorCPS (TLParen : ts) succ fail =
  parseExprCPS ts
    (\e rest -> case rest of
        (TRParen:ts') -> succ e ts'
        _             -> fail)
    fail
parseFactorCPS _ _ fail = fail

-- === Top-level Entry Point ===
parse :: String -> Either String Expr
parse input =
  let tokens = tokenize input
  in parseExprCPS tokens
       (\e rest -> if null rest
                   then Right e
                   else Left $ "Unconsumed input: " ++ show rest)
       (Left "Parse error")

-- === Example Main ===
main :: IO ()
main = do
  let input = "a + (b - 2) * 3 / x"
  print $ parse input
-- 
-- -- | Parses an arithmetic expression with + and * respecting precedence
-- parseExprCPS :: String -> (Int -> Int) -> Int
-- parseExprCPS s' f =
--   let (digits, rest) = span isDigit s'
--       num = read digits
--   in case parseString rest of
--       Nothing -> f num
--       Just ('+', a) -> parseExprCPS a (f num +)        -- Add after
--       Just ('*', a) -> parseExprCPS a (f . (num *))               -- Multiply before
-- 
-- -- | Helper to parse a character and the rest of the string
-- parseString :: String -> Maybe (Char, String)
-- parseString ""     = Nothing
-- parseString (x:xs) = Just (x, xs)
-- 
-- -- | Entry point: just apply the identity continuation
-- parseExpr :: String -> Int
-- parseExpr s = parseExprCPS s id

reverseCPS :: ([a] -> r) -> [a] -> r
reverseCPS go []    = go []
reverseCPS go (a:b) = reverseCPS (go . (++[a])) b

data Tree = Leaf Int | Node Tree Tree
sumTreeCPS :: Tree -> (Int -> r) -> r
sumTreeCPS (Leaf a)  f = f a
sumTreeCPS (Node a b) f =
    sumTreeCPS a \x ->
    sumTreeCPS b \y ->
    f (x + y)
    

