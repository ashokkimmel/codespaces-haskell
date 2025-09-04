-- TODO, REPLACE [BoolFunc a] with NonEmpty (BoolFunc a)
{-# OPTIONS_GHC -Wincomplete-patterns #-}
import Data.List (nub)
import GHC.Base (breakpoint)
import Data.Semigroup
-- import Text.Megaparsec
-- import Text.Megaparsec.Char  
import Hoogle
-- Good laws (e.g.), laws that should go at the very beggening and never trigger again
-- Implication Equivalence: p→q≡¬p∨q
-- Biconditional Equivalence:  p↔q≡(p→q)∧(q→p)

-- Simplifying laws, e.g. laws that should always fire if possible  
-- Identity Law: p∧T≡p and p∨F≡p -- the identity function
-- Idempotent Law: p∨p≡p and p∧p≡p
-- Domination Law: p∨T≡T and p∧F≡F
-- Negation Law: p∨¬p≡T and p∧¬p≡F
-- Double Negation Law: ¬(¬p)≡p
-- Absorption Law: p∨(p∧q) ≡ p and p∧(p∨q)≡p


-- Laws that have a chance of increasing complexity, to be used with care. 
-- Distributive Law: p∨(q∧r)≡(p∨q)∧(p∨r) and p∧(q∨r)≡(p∧q)∨(p∧r)  -- How to deal with effively?

-- De Morgan's Law: ¬(p∧q)≡¬p∨¬q and ¬(p∨q)≡¬p∧¬q                     
data ComplexBoolFunc a = 
    TC |
    FC  |
    VarC a | 
    NotC (ComplexBoolFunc a) | 
    OrC (ComplexBoolFunc a) (ComplexBoolFunc a) | 
    AndC (ComplexBoolFunc a) (ComplexBoolFunc a) | 
    IfThen (ComplexBoolFunc a) (ComplexBoolFunc a) |
    Iff (ComplexBoolFunc a) (ComplexBoolFunc a)
    deriving (Show,Eq,Functor)
data BoolFunc a = T | F | Var a | Not (BoolFunc a) | OrList [BoolFunc a] | AndList [BoolFunc a] deriving (Show,Eq,Functor)
safesimplify :: Eq a => BoolFunc a -> BoolFunc a 
safesimplify = recur . appEndo $ foldMap (Endo . mapeq) [demorg,identity,idempotent,domination,negation,negation2,absorbtion ,equivelent]
-- unsafesimplify = safesimplify . distriubute . safesimplify 
-- distribute (OrList (x:y)) = OrList $ map (\z -> OrList [x,z]) y
-- distribute (AndList (x:y)) = AndList $ map (\z -> OrList [x,z]) y

myparse :: ComplexBoolFunc a -> BoolFunc a 
myparse TC = T
myparse FC = F
myparse (VarC x) = Var x
myparse (NotC x) = Not $ myparse x
myparse (OrC  x y) = OrList  [myparse x,myparse y]
myparse (AndC x y) = AndList [myparse x,myparse y]
myparse (IfThen x y) = myparse (OrC (NotC x) y) 
myparse (Iff x y) = myparse (AndC (IfThen x y) (IfThen y x)) 
-- ignore :: String -> ParsecT Void String x ()
-- ignore x = string' x *> space
-- parseadd = liftA3 (const . AddC) parsecomplex (ignore "∧") parsecomplex 
-- getparen = ignore "(" *> parsecomplex <* ignore ")" 

type Symplify = forall a. Eq a => BoolFunc a -> BoolFunc a 
identity,equivelent,domination,negation,negation2,absorbtion,idempotent,demorg :: Symplify
identity (AndList x) = AndList $ filter (/=T) $ x
identity (OrList x) = OrList $ filter (/=F) $ x
identity x = x
-- equivelent :: BoolFunc a ->BoolFunc a
concatBool :: Bool -> [BoolFunc a] -> [BoolFunc a]
concatBool True  ((AndList x) : y) = x <> concatBool True y 
concatBool False ((OrList x) : y) = x <> concatBool False y
concatBool isAnd (x:y) = x:concatBool isAnd y  
concatBool _ [] = []  

equivelent (Not T) = F
equivelent (Not F) = T
equivelent (AndList [x]) = x 
equivelent (OrList  [x]) = x 
equivelent (AndList []) = error "Empty AndList, report to Ashok Kimmel akimmel@cps.edu" -- Just in case this occurs, it should never
equivelent (OrList []) = error "Empty OrList, report to Ashok Kimmel akimmel@cps.edu"   -- Just in case this occurs, it should never
equivelent (AndList x) = AndList $ concatBool True  x 
equivelent (OrList  x) = OrList  $ concatBool False x 
equivelent x = x 
recur :: Eq a => (a -> a) -> a -> a 
recur fn x = if (fn x) == x then x else recur fn (fn x)
mapeq :: (BoolFunc a -> BoolFunc a) -> BoolFunc a -> BoolFunc a 
mapeq func x = helper (func x) where 
    helper (Not y) = Not $ mapeq func y 
    helper (OrList y) = OrList $ map (mapeq func) y 
    helper (AndList y) = AndList $ map (mapeq func) y 
    helper y = y 
idempotent (AndList x) = AndList $ nub x
idempotent (OrList x) = OrList $ nub x
idempotent x = x
domination (AndList x) = if F `elem` x then F else AndList x 
domination (OrList  x) = if T `elem` x then T else OrList  x 
domination x = x
negation (AndList x) = if not $ null [z | z <- x, y <- x, z == (Not y)] then F else AndList x
negation (OrList  x) = if not $ null [z | z <- x, y <- x, z == (Not y)] then T else OrList x
negation x = x
negation2 (Not (Not x)) = x 
negation2 x = x
sfl [] a = a 
sfl (x:_) _ = x
absorbtion (AndList x) = AndList $ helper x where
    helper ((OrList y):ys) = if any (flip elem y) x then helper ys else OrList y : helper ys 
    helper (y:ys) = y : helper ys
    helper [] = [] 
absorbtion (OrList x) = OrList $ helper x where
    helper ((AndList y):ys) = if any (flip elem y) x then helper ys else OrList y : helper ys 
    helper (y:ys) = y : helper ys
    helper [] = [] 
absorbtion x = x
demorg (Not (OrList  x)) = AndList $ map Not x
demorg (Not (AndList x)) = OrList  $ map Not x
demorg x = x