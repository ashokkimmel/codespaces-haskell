{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
import GHC.TypeNats

data Zero = Zero
  deriving (Show)
data Succ x = Succ x
  deriving (Show)
type family ToPeano n where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n-1))
data Nil = Nil 
data BOne x = BOne x
  deriving (Show,Eq,Ord,Functor)
data BZero x = BZero x
  deriving (Show,Eq,Ord,Functor)
class Size a b | a -> b where 
    sizefunc :: a -> b
    sizefunc = const (size @a)
    size :: b 
instance Size Nil Zero where 
    size = Zero 
class (IsPeano a, IsPeano b) => Over2 a b c | a -> b, a -> c, b c -> a where
    over2func :: a -> b
    over2func = const $ over2 @a
    over2 :: b
instance Over2 Zero Zero Worked where over2 = Zero 
instance Over2 (Succ Zero) Zero Failed where over2 = Zero 
instance Over2 x y z => Over2 (Succ (Succ x)) (Succ y) z where 
    over2 = Succ $ over2 @x 
instance Size x y => Size (BZero x) (Succ y) where 
    size = Succ $ size @x
instance Size x y => Size (BOne x) (Succ y) where 
    size = Succ $ size @x
class KnownPeano p where
  peano :: p
instance KnownPeano Zero where peano = Zero
instance KnownPeano x => KnownPeano (Succ x) where peano = Succ peano
toPeano :: (KnownPeano (ToPeano n)) => ToPeano n
toPeano = peano
data Worked = Worked 
data Failed = Failed  
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
  changebit :: IsBit b => b -> a b
instance BitConstruct BZero where 
  changebit = BZero 
instance BitConstruct BOne where 
  changebit = BOne 
class (IsPeano bitnum, IsPeano a, IsBit b) => ToBit bitnum a b | bitnum -> b 
instance ToBit Zero Zero Nil
instance (ToBit x y z, (Doubled x x2)) => ToBit (Succ x) x2  (BZero z)


-- instance (ToBit x y (BZero z)) => ToBit (Succ x) (Succ x)  (BOne z)
-- instance (ToBit x (BZero y) z, (Div2 y2 x)) => ToBit (Succ x) y2 (bitchange z)
-- class ToNBit bitnum a b Zero => PeanoToBit bitnum a b | a -> b where 
--   toNBit :: b 
--   toNBit = whichBit @b 
--   toNBitfunc :: a -> b 
--   toNBitfunc = const $ whichBit @b 
-- instance ToNBit bitnum a b Zero => PeanoToBit bitnum a b
-- class PeanoToBit (ToPeano 32) a b => To16Bit a b | a -> b where 
--   toBit :: b 
--   toBit = whichBit @b
-- instance PeanoToBit (ToPeano 32) a b => To16Bit a b
