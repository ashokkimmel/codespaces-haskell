-- module Ex10Bork (
--     Env, emptyEnv, Value(..), HaskellProc(..), Expr(..), eval, def
-- ) where
{-# LANGUAGE TypeFamilies #-}
import qualified Data.Map as Map
import Data.Functor.Foldable 
import Data.Bifunctor 
------------------------------------------------------------------------------
-- Data Definitions: the (non-CPS) Bork language
------------------------------------------------------------------------------

-- | An environment is a mapping of names to values
type Env = Map.Map String Value
emptyEnv = Map.empty

-- | Values in Bork include:
data Value = Nil | T | F            -- null values and booleans
           | Num Int                -- integers
           | Procedure HaskellProc  -- procedures (closures), see below
           | Error String           -- errors    
          deriving (Eq, Show)

-- | Bork procedures are represented using Haskell functions.
--   A procedure takes a list of arguments, and returns a single value.
--   This representation is different from the representations we used
--   in previous exercises and assignment, and is necessary for the CPS
--   transform in Part 3. We also define methods for comparing and
--   printing procedures.
data HaskellProc = Proc ([Value] -> Value)
instance Show HaskellProc where  
    show x = "[Can't Show Procedure]"
instance Eq HaskellProc where  
    x == y = False

-- | Expressions in Bork include:
data Expr = Literal Value           -- literal values
          | Plus Expr Expr          -- builtin "plus" function
          | Times Expr Expr         -- builtin "times" function
          | Equal Expr Expr         -- builtin checks for equality
          | Var String              -- variable names
          | If Expr Expr Expr       -- if statements
          | Lambda [String] Expr    -- function definitions
          | App Expr [Expr]         -- function applications
          deriving (Eq, Show)
data ExprF r_ajhU
  = LiteralF Value |
    PlusF r_ajhU r_ajhU |
    TimesF r_ajhU r_ajhU |
    EqualF r_ajhU r_ajhU |
    VarF [Char] |
    IfF r_ajhU r_ajhU r_ajhU |
    LambdaF [[Char]] r_ajhU |
    AppF r_ajhU [r_ajhU]
  deriving (Functor, Foldable, Traversable)
type instance Base Expr = ExprF
instance Recursive Expr where
  project (Literal x_ajhV) = LiteralF x_ajhV
  project (Plus x_ajhW x_ajhX) = (PlusF x_ajhW) x_ajhX
  project (Times x_ajhY x_ajhZ) = (TimesF x_ajhY) x_ajhZ
  project (Equal x_aji0 x_aji1) = (EqualF x_aji0) x_aji1
  project (Var x_aji2) = VarF x_aji2
  project (If x_aji3 x_aji4 x_aji5) = ((IfF x_aji3) x_aji4) x_aji5
  project (Lambda x_aji6 x_aji7) = (LambdaF x_aji6) x_aji7
  project (App x_aji8 x_aji9) = (AppF x_aji8) x_aji9
instance Corecursive Expr where
  embed (LiteralF x_ajia) = Literal x_ajia
  embed (PlusF x_ajib x_ajic) = (Plus x_ajib) x_ajic
  embed (TimesF x_ajid x_ajie) = (Times x_ajid) x_ajie
  embed (EqualF x_ajif x_ajig) = (Equal x_ajif) x_ajig
  embed (VarF x_ajih) = Var x_ajih
  embed (IfF x_ajii x_ajij x_ajik) = ((If x_ajii) x_ajij) x_ajik
  embed (LambdaF x_ajil x_ajim) = (Lambda x_ajil) x_ajim
  embed (AppF x_ajin x_ajio) = (App x_ajin) x_ajio
------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

-- | The interpreter `eval` for Bork, which takes an environment
--   and an expression, and returns the evaluated value.
unzip' :: Functor f => f (a,b) -> (f a, f b) -- This uses an old version of base without Data.Functor.unzip. If you are updated, I suggest you use that instead.
unzip' xs = (fst <$> xs, snd <$> xs)         -- This uses an old version of base without Data.Functor.unzip. If you are updated, I suggest you use that instead.

evalAlg :: Env -> Expr ->  ExprF Value -> Value
evalAlg _   _ (LiteralF v) = v
evalAlg _   _ (PlusF a b)  = case (a,b) of
    (Num x, Num y) -> Num (x + y)
    _              -> Error "plus"
evalAlg _   _ (TimesF a b) = case (a,b) of
    (Num x, Num y) -> Num (x * y)
    (c, d)         -> Error "times"
evalAlg _   _ (EqualF a b) = if a==b then T else F
evalAlg env _ (VarF name)  = case (Map.lookup name env) of
    Just a  -> a
    Nothing -> Error "lookup"
evalAlg env _ (IfF cond expr alt) = if cond == T
    then expr
    else alt
evalAlg env (Lambda params body) _ = Procedure $ Proc $ \vargs ->
    let newEnv = Map.union (Map.fromList (zip params vargs)) env
    in eval newEnv body
evalAlg env (App _ args) (AppF procval vargs) = 
    case procval of
        Procedure (Proc f) ->  f vargs
        _                  -> Error (show args)

eval env = para $ uncurry (evalAlg env) . first embed . unzip'
------------------------------------------------------------------------------
-- Environment Definition
------------------------------------------------------------------------------

-- | Create a new environment from a set of bindings (pairs of names to
--   expressions to be evaluated). These expressions may be recursive:
--   in other words, the expression might reference names that are 
--   being defined. In order to support recursion, notice that we
--   are passing the `env` (currently being created) as the environment
--   in the call to `eval`! This is similar to how we use `letrec` in
--   Racket, but relies on Haskell's lazy evaluation.
def :: [(String, Expr)] -> Env
def bindings = 
    let env = Map.fromList (map (\(n,e) -> (n, (eval env e))) bindings)
    in env

------------------------------------------------------------------------------
-- Example Bork Programs
------------------------------------------------------------------------------

-- | Example: apply the identity function to the number 3
example1 = eval emptyEnv (App (Lambda ["a"] (Var "a")) [Literal $ Num 3])

-- | Example: apply a function that returns 10 plus the second argument
--            to the arguments [1, 2]
example2 = eval emptyEnv (App (Lambda ["a", "b"] (Plus (Literal $ Num 10) (Var "b")))
                              [Literal $ Num 1, Literal $ Num 2])
-- | Example: if statement expression
example3 = eval emptyEnv (If (Equal (Literal F) (Literal F))
                             (Literal T)
                             (Literal F))
-- | Example: creating a function using `def`
sub1Env = def [("sub1", Lambda ["n"] (Plus (Var "n") (Literal $ Num (-1))))]
example4 = eval sub1Env (App (Var "sub1") [Literal $ Num 5])

---- | Example: factorial
facEnv = def [("fac", Lambda ["n"]
                        (If (Equal (Var "n") (Literal $ Num 0))
                            (Literal $ Num 1)
                            (Times (Var "n") (App (Var "fac")
                               [(Plus (Var "n") (Literal $ Num (-1)))]))))]
example5 = eval facEnv (App (Var "fac") [Literal $ Num 5])

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    print example1
    print example2
    print example3
    print example4
    print example5