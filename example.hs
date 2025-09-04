{-# LANGUAGE FunctionalDependencies, TypeFamilies #-}

newtype WrappedB a = WrappedB { unwrapB :: B a }
class C a b | a -> b 
class C' a where 
  type B a :: *
instance C' a => C a (WrappedB a)  -- you can't use a type synonym family in an instance

newtype Wrap a b = Wrap { unWrap :: a }
instance C a b => C' (Wrap a b) where
    type B (Wrap a b) = b