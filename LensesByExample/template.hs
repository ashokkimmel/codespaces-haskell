{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
myfolded :: Foldable f => Fold (f a) a 
myfolded = folded
myto :: (a -> b) -> Getter a b 
myto func construct value = contramap func (construct (func value))
myfolding ::  Foldable f => (s -> f a) -> Fold s a
myfolding a = myto a . myfolded