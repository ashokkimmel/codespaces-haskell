{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- Good laws (e.g.), laws that should go at the very beggening and never trigger again
-- Implication Equivalence: p→q≡¬p∨q
-- Biconditional Equivalence:  p↔q≡(p→q)∧(q→p

-- Simplifying laws, e.g. laws that should always fire if possible  
-- Identity Law: p∧T≡p and p∨F≡p -- the identity function
-- Idempotent Law: p∨p≡p and p∧p≡p
-- Domination Law: p∨T≡T and p∧F≡F
-- Negation Law: p∨¬p≡T and p∧¬p≡F
-- Double Negation Law: ¬(¬p)≡p
-- Absorption Law: p∨(p∧q)≡p and p∧(p∨q)≡p

-- Laws that have a chance of increasing complexity, to be used with care. 
-- Distributive Law: p∨(q∧r)≡(p∨q)∧(p∨r) and p∧(q∨r)≡(p∧q)∨(p∧r)
-- De Morgan's Law: ¬(p∧q)≡¬p∨¬q and ¬(p∨q)≡¬p∧¬q

data ComplexBoolFunc a = 
    TC |
    FC  |
    VarC a | 
    NotC a | 
    OrC (ComplexBoolFunc a) (ComplexBoolFunc a) | 
    AndC (ComplexBoolFunc a) (ComplexBoolFunc a) | 
    OrListC [ComplexBoolFunc a] | 
    AndListC [ComplexBoolFunc a] |
    IfThen (ComplexBoolFunc a) (ComplexBoolFunc a) |
    Iff (ComplexBoolFunc a) (ComplexBoolFunc a)
    deriving (Show,Eq,Functor)
data BoolFunc a = T | F | Var a | Not (BoolFunc a) | OrList [BoolFunc a] | AndList [BoolFunc a] deriving (Show,Eq,Functor)
type Symplify = forall a. Eq a => BoolFunc a -> BoolFunc a 
identity :: Symplify
identity (AndList x) = AndList $ map identity . filter (/=T) $ x
identity (OrList x) = OrList $ map identity . filter (/=F) $ x
identity x = x
-- equivelent :: BoolFunc a ->BoolFunc a  -- This merely converts the representation e.g. (Not T) -> F or Add (x (Add y z)) -> AddList [x,y,z] 
equivelent (Not T) = F
equivelent (Not F) = T
equivelent (AndList [x]) = equivelent x 
equivelent (OrList [x]) = equivelent x 
equivelent x = x 
-- equivelent (And x y) = [x,y,z]
recur :: Eq a => (a -> a) -> a -> a 
recur fn x = if (fn x) == x then x else recur fn (fn x)
mapeq :: (BoolFunc a -> BoolFunc a) -> BoolFunc a -> BoolFunc a 
mapeq func x = helper (func x) where 
    helper (Not y) = mapeq func y 
    helper (OrList y) = OrList $ map (mapeq func) y 
    helper (AndList y) = AndList $ map (mapeq func) y 
    helper x = x 