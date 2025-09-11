{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module BasicDefs where 
import Control.Applicative (liftA2)
import GHC.IsList
data Empty a = Empty deriving (Show,Eq,Ord,Functor,Foldable)
instance Applicative Empty where 
  pure _ = Empty
  (<*>) _ _ = Empty
  liftA2 _ _ _ = Empty
instance Monad Empty where 
  (>>=) _ _ = Empty
data VList f a = VList a (f a) deriving (Eq,Ord,Functor,Foldable)
vsplit (VList a b)= (a,b)
vhead = fst . vsplit 
vtail = snd . vsplit 
instance (IsVec f,IsList (f a),Show a) => Show (VList f a) where
  show s = "Length: " ++ show (toNum s) ++ ", Elements: " ++ show (toList s) 

instance Applicative f => Applicative (VList f) where 
  pure a = VList a (pure a)
  (VList a b) <*> (VList c d) = VList (a c) (b <*> d)  
  liftA2 fn (VList a b) (VList c d) = VList (fn a c) (liftA2 fn b d)  

class Applicative f => IsVec f where 
  toNum :: (f a) -> Int 
  transpose :: IsVec b => (f (b c)) -> (b (f c))
  vtoList :: (f a) -> [a]
  vfromList :: [a] -> (f a)

instance IsVec Empty where 
  transpose _ = pure Empty
  toNum _ = 0
  vtoList _ = mempty
  vfromList _ = Empty 
instance IsVec a => IsList (a b) where 
  type Item (a b) = b
  toList = vtoList
  fromList = vfromList
data Nil 
data Succ a 
u = undefined
class IsVec b => PeanoVec a b | a -> b , b -> a where 
  toPeano :: (b s) -> a
  toPeano _ = u  
  toVec :: a -> (b ()) 
  toVec _ = pure ()

instance PeanoVec Nil Empty
instance PeanoVec a b => PeanoVec (Succ a) (VList b) 
mymulth :: (Applicative f,Foldable f,Num a) => f a -> f a -> a 
mymulth a = sum . liftA2 (*) a
instance IsVec f => IsVec (VList f) where 
  toNum (VList _ a) = succ (toNum a)
  vtoList (VList a b)= a:vtoList b
  transpose (VList a b) = liftA2 VList a (transpose b) 
  vfromList (a:b)= VList a (vfromList b)
  vfromList _ = errorWithoutStackTrace "Vec.fromList: list less then vector size"

type E = Empty 
type V = VList
type V2 a = V (V a)
type V4 a = V2 (V2 a)
type V8 a = V4 (V4 a)
type V16 a = V8 (V8 a)
type V32 a = V16 (V16 a)
