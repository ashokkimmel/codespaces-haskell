--{-# LANGUAGE OverloadedLists #-}
--import Data.Matrix 
--import Data.List (nub)
--data Elem = Fail | Success | Rolls (Int,Int) 
--toElem :: Int -> Elem 
--toElem 10 = Fail 
--toElem 11 = Success
--toElem x = Rolls ((x-1) `div` 3, (x-1) `mod` 3)
--builder' :: Elem -> Elem -> Rational 
--builder' Fail Fail = 1
--builder' Fail _    = 0
--builder' Success Success = 1
--builder' Success _       = 0
--builder' (Rolls (_,2)) Success = 11 % 20 
--builder' (Rolls _) Success = 1 % 20 
--builder' (Rolls (0,_)) Fail = 0 
--builder' (Rolls _) Fail = 9 % 20 
--builder' (Rolls (a,b)) (Rolls (c,d))
{-# LANGUAGE TemplateHaskell #-}
module GraphTheory where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr (pprint) -- for pretty-printing Info

runexpr :: Show a => a -> Q [Dec]
runexpr info= do
    runIO $ print info 
    return []
