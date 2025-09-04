{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE RebindableSyntax #-}
import Prelude hiding ((+),(*),(-),negate)
import qualified Prelude as P
import GHC.TypeNats
import Protolude (identity)

data Zero = Zero
  deriving (Show)
data Succ x = Succ x
  deriving (Show)
type family ToPeano n where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n-1))
data Nil = Nil deriving (Show,Eq,Ord)
data BOne x = BOne x
  deriving (Show,Eq,Ord,Functor)
data BZero x = BZero x
  deriving (Show,Eq,Ord,Functor)
class (IsBit a,IsPeano b) => Size a b | a -> b where 
    sizefunc :: a -> b
    sizefunc = const (size @a)
    size :: b 
    size = whichPeano @b
instance Size Nil Zero where 
    size = Zero 
class (IsPeano a, IsPeano b) => Over2 a b | a -> b 
instance Over2 Zero Zero 
instance Over2 (Succ Zero) Zero 
instance Over2 x y => Over2 (Succ (Succ x)) (Succ y) 
instance Size x y => Size (BZero x) (Succ y)  
instance Size x y => Size (BOne x) (Succ y) 
class KnownPeano p where
  peano :: p
instance KnownPeano Zero where peano = Zero
instance KnownPeano x => KnownPeano (Succ x) where peano = Succ peano
toPeano :: (KnownPeano (ToPeano n)) => ToPeano n
toPeano = peano
class (IsPeano a, IsPeano b) => Doubled a b | a -> b, b -> a where 
  doublefunc :: a -> b 
  doublefunc = const $ double @a 
  double :: b 
  double = whichPeano @b 
instance Doubled Zero Zero 
instance Doubled x y => Doubled (Succ x) (Succ (Succ y)) 
class IsPeano x where 
  whichPeano :: x
instance IsPeano Zero where whichPeano = Zero 
instance IsPeano x => IsPeano (Succ x) where whichPeano = Succ $ whichPeano @x 
class IsBit x where 
  whichBit :: x
instance IsBit Nil where whichBit = Nil 
instance (IsBit x) => IsBit (BZero x) where whichBit = BZero (whichBit @x) 
instance (IsBit x) => IsBit (BOne x) where whichBit = BOne (whichBit @x) 
class (IsPeano x, BitConstruct y) => Mod2 x (y :: * -> *) | x -> y where 
instance Mod2 Zero BZero
instance Mod2 (Succ Zero) BOne
instance Mod2 x y => Mod2 (Succ (Succ x)) y  
class (IsPeano x, IsPeano y) => Div2 x y | x -> y 
instance Div2 Zero Zero 
instance Div2 (Succ Zero) Zero 
instance Div2 x y => Div2 (Succ (Succ x)) (Succ y)  
class BitConstruct a where 
  changebit :: (IsBit b, IsBit (a b)) => b -> a b
instance BitConstruct BZero where 
  changebit = BZero 
instance BitConstruct BOne where 
  changebit = BOne 
class (IsPeano bitnum, IsPeano sum, IsBit bit, IsPeano remainder) => ToBit bitnum sum bit remainder | bitnum sum -> bit remainder
instance IsPeano x => ToBit Zero x Nil x
instance (ToBit a b c d, Mod2 d x, IsBit (x c), Over2 d final) => ToBit (Succ a) b (x c) final
type AmountofBits = ToPeano 16
class ToBit bitnum a b Zero => PeanoToBit bitnum a b | bitnum a -> b where 
instance ToBit bitnum a b Zero => PeanoToBit bitnum a b
class PeanoToBit AmountofBits a b => ToNBit a b | a -> b where 
  toBitfunc :: a -> b 
  toBitfunc = const $ whichBit @b 
instance PeanoToBit AmountofBits a b => ToNBit a b
toBint :: forall n x. (KnownPeano (ToPeano n),ToNBit (ToPeano n) x) => x 
toBint = toBitfunc $ toPeano @n
-- class (IsBit a, IsBit b,IsBit c, Size a x, Size b y, Size c z, x ~ y, y ~ z) => Multiply a b c where 
--   multfunc :: a -> b -> c 
--   multfunc = const . const $ mult @a @b 
--   mult :: c
data Zero' 
data One' 
class DataBool x
instance DataBool Zero' 
instance DataBool One' 
class (DataBool starter, BitConstruct bit1, BitConstruct bit2, BitConstruct bitresult, DataBool remainder) => Add1Bit starter bit1 bit2 bitresult remainder | starter bit1 bit2 -> bitresult remainder
instance Add1Bit Zero' BZero BZero BZero Zero'
instance Add1Bit One'  BZero BZero BOne  Zero'
instance Add1Bit Zero' BOne  BZero BOne  Zero'
instance Add1Bit Zero' BZero BOne  BOne  Zero'
instance Add1Bit One'  BOne  BZero BZero One'
instance Add1Bit One'  BZero BOne  BZero One'
instance Add1Bit Zero' BOne  BOne  BZero One'
instance Add1Bit One'  BOne  BOne  BOne  One'

class (IsBit a, IsBit b, IsBit c, DataBool d) => Add a b c d | a b -> c d
instance Add Nil Nil Nil Zero' 
instance (IsBit (x a), IsBit (y b), IsBit (z c), Add a b c d, Add1Bit d x y z letover) => Add (x a) (y b) (z c) letover 
class (IsBit a, IsBit b, IsBit c) => AddG a b c | a b -> c where 
    (+) :: a -> b -> c 
    (+) _ _ = whichBit @c
    infixl 6 +
instance (IsBit a, IsBit b, IsBit c, Add a b c d) => AddG a b c


class (IsPeano divisor,IsPeano dividend,IsPeano quotient,IsPeano remainder) => Division divisor dividend quotient remainder
class ToInt a where 
  toInt :: a -> Int 
  toInt _ = intVal @a 
  intVal :: Int
instance ToInt Zero where 
  intVal = 0
instance ToInt x => ToInt (Succ x) where 
  intVal = 1 P.+ intVal @x 
instance ToInt Nil where 
  intVal = 0
instance (Size a b,ToInt b,ToInt a) => ToInt (BZero a) where 
  intVal = (intVal @a)
instance (Size a b,ToInt b,ToInt a) => ToInt (BOne a) where 
  intVal = (2 ^ (intVal @b)) P.+ (intVal @a)
class (IsPeano number, IsBit old, IsBit new) => BitShift number old new | number old -> new 
class (IsBit x,IsBit y) => Multiply2 x y | x -> y 
instance Multiply2 Nil (BZero Nil)
instance Multiply2 x y => Multiply2 (BOne x) (BOne y)
instance Multiply2 x y => Multiply2 (BZero x) (BZero y)
class (IsBit x,IsBit y) => Remove x y | x -> y
instance (IsBit (BZero x), IsBit x) => Remove (BZero x) x
instance (IsBit (BOne x), IsBit x) => Remove (BOne x) x
instance (IsBit x) =>  BitShift Zero x x 
instance (BitShift x y z, Remove z z', Multiply2 z' z'') => BitShift (Succ x) y z''
class (IsBit a,IsBit b) => ToZero a b | a -> b 
instance ToZero Nil Nil 
instance ToZero a b => ToZero (BOne a) (BZero b)
instance ToZero a b => ToZero (BZero a) (BZero b)
class (IsBit x,IsBit y,IsBit z) => BitMultiply x y z | x y -> z where 
  (*) :: x -> y -> z 
  (*) _ _ = whichBit @z 
  infixl 7 *
class (IsPeano a,IsBit b) => ToMax a b | a -> b where 
  maxNum :: b 
  maxNum = whichBit @b 
instance ToMax Zero Nil 
instance ToMax a b => ToMax (Succ a) (BOne b) 
instance (IsBit x,ToZero x y) => BitMultiply Nil x y  
-- z is determined by x and y
-- xs is determined by x 
-- y' is determined by xs and y
-- z' is determined by y' and z 
instance (BitMultiply x y z, Size x xs,BitShift xs y y',AddG y' z z',IsBit z') => BitMultiply (BOne x) y z'
instance BitMultiply x y z => BitMultiply (BZero x) y z

class (IsBit a,IsBit b) => BitWiseNegate a b | a -> b 
instance BitWiseNegate Nil Nil 
instance (BitWiseNegate a b) => BitWiseNegate (BZero a) (BOne b)
instance (BitWiseNegate a b) => BitWiseNegate (BOne a) (BZero b)
class (IsPeano a,IsBit b) => AddZeroes a b where 
  type WZeroes a b
instance IsBit x => AddZeroes Zero x where 
  type WZeroes Zero x = x
instance (IsPeano (Succ a), IsBit b,AddZeroes a b) => AddZeroes (Succ a) b where 
  type WZeroes (Succ a) b = (BZero (WZeroes a b))


type Bit1 = BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BZero (BOne Nil)))))))))))))))

class (IsBit a,IsBit b) => Negate a b | a -> b where 
  negate :: a -> b 
  negate _ = whichBit @b 
instance (IsBit a, IsBit b', BitWiseNegate a b,AddG Bit1 b b') => Negate a b'
class ToSInt x where 
  toSt :: Int 
  toS :: x -> Int
  toS _ = toSt @x 
instance (BitWiseNegate x x',ToInt x') => ToSInt (BOne x) where 
  toSt = P.negate $ intVal @x' P.+ 1
instance ToInt x => ToSInt (BZero x) where 
  toSt = intVal @x
data TrueT 
data FalseT
type family Equals a b where
  Equals a a = TrueT
  Equals a b = FalseT
class ToBool a where 
  boolV :: Bool 
instance ToBool TrueT  where boolV = True
instance ToBool FalseT where boolV = False
(===) :: forall a b c. (ToBool c, c ~ (Equals a b)) => a -> b -> Bool
a === b = boolV @c
infixl 4 ===
class IsType a where 
  t :: a 
instance IsType a where t = undefined
class (IsBit a,IsBit b,IsBit c) => Subtract a b c | a b -> c where 
  (-) :: a -> b -> c
  (-) _ _ = whichBit @c 
  infixl 6 -
instance (Negate b b',AddG a b' c,IsBit a,IsBit b,IsBit c) => Subtract a b c
class FlipBit x y | x -> y where 
  flipBit :: x -> y 
instance FlipBit (BOne x) (BZero x) where 
  flipBit (BOne x) = (BZero x)
instance FlipBit (BZero x) (BOne x) where 
  flipBit (BZero x) = (BOne x)
minimumBit = flipBit $ toBint @0
maximumBit = flipBit $ maxNum @AmountofBits
