{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
import qualified GHC.TypeLits as TL
import qualified TypeLevelInt as TI
import Data.Functor.Classes
import Data.Semigroup (Endo(..),stimes)
import Data.Functor.Identity
import Data.Proxy
type Sort :: [(TL.Symbol, TI.Int')] -> [(TL.Symbol, TI.Int')]
type family Sort a where
    Sort '[a] = '[a]
    Sort '[] = '[]
    Sort a = SortH (Halve a)
type SortH :: ([(TL.Symbol, TI.Int')],[(TL.Symbol, TI.Int')]) -> [(TL.Symbol, TI.Int')]
type family SortH a where
    SortH '(a, b) = Merge (Sort a) (Sort b)
type Merge :: [(TL.Symbol, TI.Int')] -> [(TL.Symbol, TI.Int')] -> [(TL.Symbol, TI.Int')]
type family Merge a b where
    Merge '[] a = a
    Merge a '[] = a
    Merge (a ': c) (b ': d) = MergeH (Fst a `TL.CmpSymbol` Fst b) a b c d

type Fst :: (a,b) -> a
type family Fst a where Fst '(a,_) = a

type family MergeH ord a b c d where
    MergeH EQ '(a,b) '(_,d) e f = '(a,b TI.+ d) ': Merge e f
    MergeH LT a b c d = a ': Merge c (b ': d)
    MergeH GT a b c d = b ': Merge (a ': c)  d
type Halve :: [a] -> ([a],[a])
type family Halve a where
    Halve a = Span (Length a `TL.Div` 2) a

type Span :: TL.Nat -> [a] -> ([a],[a])
type family Span n xs where
    Span 0 xs = '( '[], xs)
    Span n (x ': xs) = Add x (Span (n TL.- 1) xs)
type Add :: a -> ([a],b) -> ([a],b)
type family Add a b where
    Add a '(d,c) = '(a ': d,c)

type Length :: [a] -> TL.Nat
type family Length xs where
    Length '[] = 0
    Length (x ': xs) = 1 TL.+ Length xs
type family Has0 a where
    Has0 '[] = 'False
    Has0 ('(_,TI.Pos 0) ': _) = 'True
    Has0 (_ ': b) = Has0 b
type family Unzero a where
    Unzero '[] = '[]
    Unzero ('(_,TI.Pos 0) ': b) = Unzero b
    Unzero (a ': b) = a ': Unzero b

type ValidDimension a = (a ~ Sort a, Has0 a ~ 'False)
class HasString f where
    getString :: String
instance HasString '[] where
    getString = ""
instance (TL.KnownSymbol s,TI.ToInt n,HasString p) => HasString ('(s,n) ': p) where
    getString = TL.symbolVal (Proxy @s) ++ "^" ++ show (TI.intval @n) ++ getString @p
newtype Dimension a b = MkDimension b
    deriving stock (Functor,Foldable,Traversable)
    deriving newtype (Eq,Ord)
instance (Show b,HasString a) => Show (Dimension a b) where
    show (MkDimension a) =  show a ++ getString @a
dimension :: forall a b. b -> Dimension (Sort (Unzero a)) b
dimension = MkDimension

validateDimension :: Dimension a b -> Dimension (Sort (Unzero a)) b
validateDimension (MkDimension a) = MkDimension a

(!*) :: Num n => Dimension a n -> Dimension b n -> Dimension (Merge a b) n
(MkDimension a) !* (MkDimension b) = MkDimension (a * b)

undimension :: Dimension '[] a -> a
undimension (MkDimension a) = a
type Replace :: TL.Symbol -> TL.Symbol -> [(TL.Symbol, TI.Int')] -> [(TL.Symbol, TI.Int')]
type family Replace s t x where
    Replace _ _ '[] = '[]
    Replace a b ('(a,c) ': d) = '(b,c) ': Replace a b d
type family Lookup a b where
    Lookup _ '[]  = 'Nothing
    Lookup a ('(a,b) ': _) = 'Just b
    Lookup a (_ ': b) = Lookup a b

transform :: forall s t x a. TI.ToInt (Lookup s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transform (fun,invfun) (MkDimension a) = let times = TI.intval @(Lookup s x) in
    if  | times == 0 -> MkDimension a
        | times > 0 -> MkDimension $ appEndo (stimes times (Endo fun)) a
        | times < 0 -> MkDimension $ appEndo (stimes (negate times) (Endo invfun)) a

type family UnJust a where
    UnJust ('Just a) = a
    UnJust 'Nothing = TI.Pos 0
transformpos :: forall s t x a. (TL.KnownNat (TI.ToNatural (UnJust (Lookup s x)))) => (a -> a) -> Dimension x a -> Dimension (Sort (Replace s t x)) a
transformpos fun (MkDimension a) = let times = TL.natVal (Proxy @(TI.ToNatural (UnJust (Lookup s x)))) in
    MkDimension $ appEndo (stimes times (Endo fun)) a