{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.TypeNats
import Control.Lens hiding (Control.Lens.Tutorial)
boardlength :: Natural 
boardlength = 10
--type Boardwrapper a = (Vec boardlength (Vec boardlength Attack))
data Board = Board { _pl1Atk :: [Attack]
                   , _pl2Atk :: [Attack]
                   , _pl1Ship :: [Ship]
                   , _pl2Ship ::[Ship]}
data Attack = Hit Int Int | Miss Int Int 


x :: Control.Lens.Lens' Attack Int 
x fn (Hit x y) = fmap (flip Hit y) $ fn x
x fn (Miss x y) = fmap (flip Miss y) $ fn x
y :: Control.Lens.Lens' Attack Int 
y fn (Hit x y) = fmap (flip Hit x) $ fn y
y fn (Miss x y) = fmap (flip Miss x) $ fn y



data Ship = Ship 
data V a = V a

type family Vec n a where
  Vec 0 a = a
  Vec n a = Vec (n-1) (V a)
