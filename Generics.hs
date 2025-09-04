{-# LANGUAGE  DataKinds,ExistentialQuantification,TemplateHaskell #-}
import Data.Singletons
$(singletons [d|
  data Two = A | B 
    deriving (Show, Eq,Ord,Enum,Bounded)
  |])