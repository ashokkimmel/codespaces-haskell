{-# LANGUAGE TemplateHaskell #-}
import TemplateHaskell
$x 
newtype MyType = MyType { myField :: Int }
$(makelens ''MyType)