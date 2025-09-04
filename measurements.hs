{-# LANGUAGE NoImplicitPrelude,RebindableSyntax #-}

import Data.Coerce (coerce) 

import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
{- data Unit = 
    Tonnes Int|
    Grams Int |
    Calories Int |
    Population Int |
    Day Int|
    Years Int |   
type D = Base Double
instance Num a => Num (Base a) where 
    (-) = coerce (-)
    negate = coerce negate 
    (+) = coerce (+)
    (*) = coerce (*)
    abs = coerce abs 
    signum = coerce signum -}

sugar =  178930000.0 *~ tonne 
calorieamount = (4 *~ calorie) / (1 *~ gram) 
calorieintake = (1 *~ day) / (2000 *~ calorie)
todimension a = (a *~ meter) / (1 *~ meter)
population = todimension 8231613070 
a /. b = (/) . todimension
a *. b = (*) . todimension

amount = (sugar * calorieamount * calorieintake / population) 
