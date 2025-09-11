{-# LANGUAGE NoImplicitPrelude #-}
import Prelude hiding (readFile,writeFile)
import Data.ByteString (readFile,writeFile)
import Data.ByteString.Base64
fromRight ~(Right a) = a 
main1 = do 
    x <- readFile "synergismfile.txt"
    writeFile "decoded.txt" (fromRight (decode x))
main2 = do 
    x <- readFile "decoded.txt"
    writeFile "brockenfile.txt" ( (encode x))