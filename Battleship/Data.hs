{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Data where
import Data.Set (Set,toList)
import Control.Lens hiding (Control.Lens.Tutorial)
data Attack = Hit Int Int | Miss Int Int deriving (Show,Eq,Ord)
newtype Ship = Ship {_partsLeft :: Set (Int,Int)} deriving (Eq)
$(makeLenses ''Ship)
instance Show Ship where 
    show x =  "Remaining positions: " ++ show (toList (_partsLeft x))
data Vertical = V | H
data Single = Single { _attacks :: Set Attack
                     , _eships :: [Ship]
            , _whoseturn :: Bool}
attacks :: Lens' Single (Set Attack)
attacks f_atyx (Single x1_atyy x2_atyz x3_atyA) = (fmap (\ y1_atyB -> ((Single y1_atyB) x2_atyz) x3_atyA))
      (f_atyx x1_atyy)
{-# INLINE attacks #-}
eships :: Lens' Single [Ship]
eships f_atyC (Single x1_atyD x2_atyE x3_atyF)
  = (fmap (\ y1_atyG -> ((Single x1_atyD) y1_atyG) x3_atyF))
      (f_atyC x2_atyE)
{-# INLINE eships #-}
whoseturn :: Lens' Single Bool
whoseturn f_atyH (Single x1_atyI x2_atyJ x3_atyK)
  = (fmap (\ y1_atyL -> ((Single x1_atyI) x2_atyJ) y1_atyL))
      (f_atyH x3_atyK)
{-# INLINE whoseturn #-}

{- }
data Board = Board { _pl1Atk :: Set Attack
                   , _pl2Atk :: Set Attack
                   , _pl1Ship :: [Ship]
                   , _pl2Ship :: [Ship]
                   , _turn :: Player
                   , _winner :: Player
                   , _shipsize :: Maybe Int} 
$(makeLenses ''Board)
baseUI = UI "" "" True True emptyBoard 
data UI = UI { _pl1Name :: String
             , _pl2Name :: String
             , _setup :: Bool
             , _lookingatscreen :: Bool
             , _board :: Board} deriving (Show)
$(makeLenses ''UI)

emptyBoard = Board [] [] [Ship [(1,1),(2,2)]] [Ship [(1,1),(2,2)]] Pl1 None (Just 2)
instance Show Board where 
    show x = unlines [header,pl1attacks,pl2attacks,pl1ships,pl2ships,whosemove] where 
        header = "Board:"
        pl1attacks = "Player 1 attacks = " ++ show (toList (_pl1Atk x)) 
        pl2attacks = "Player 2 attacks = " ++ show (toList (_pl2Atk x))
        pl1ships = "Player 1 ships = " ++ show (_pl1Ship x)
        pl2ships = "Player 2 ships = " ++ show (_pl2Ship x)
        whosemove = "Turn = " ++ show (_turn x)

data Player = None | Pl1 | Pl2 deriving (Eq,Show,Ord)
x :: Lens' Attack Int 
x fn (Hit x y) = fmap (flip Hit y) $ fn x
x fn (Miss x y) = fmap (flip Miss y) $ fn x
y :: Lens' Attack Int 
y fn (Hit x y) = fmap (flip Hit x) $ fn y
y fn (Miss x y) = fmap (flip Miss x) $ fn y
loc :: Lens' Attack (Int,Int)
loc fn (Hit x y) = fmap (uncurry Hit) $ fn (x,y)
loc fn (Miss x y) = fmap (uncurry Miss) $ fn (x,y)
-}