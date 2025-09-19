-- type Cont a b = (a -> b) -> b 
-- --newtype Odd a b = Odd (a,[(a,b)]) deriving (Show,Functor,Foldable)
-- type CalculatorParser = forall r. String -> (String -> Tree -> r) -> r
-- data Tree = Plus Tree Tree | Sub Tree Tree | Times Tree Tree | Divide Tree Tree | Num Float | Exp Tree Tree  
-- parseplus :: CalculatorParser 
-- parseplus lst f = parsemuldiv lst (f )
-- parsemuldiv :: CalculatorParser
-- parsemuldiv = undefined
-- 
-- --parsewleftover :: String -> (Tree,String)
-- --parsewleftover x = parseparen x undefined
-- -- ((String,Tree) -> (String,Tree)) -> (String,Tree)
data OneZero = Zero | One deriving (Eq)
instance Num OneZero where 
    fromInteger 0 = Zero  
    fromInteger 1 = One 
myid :: OneZero -> Bool
myid 0 = False 
myid 1 = True  
-- 