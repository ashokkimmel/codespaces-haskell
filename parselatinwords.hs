{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use let" #-}
import Data.Maybe (mapMaybe)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)

uncons [] = Nothing 
uncons (a:b) = Just (a,b)
main = do 
    x <- readFile "latinwords.txt"
    x <- pure $ mapMaybe uncons (lines x)
    x <- pure $ filter (isDigit . fst) x
    x <- pure $ map ((\(a:b) -> a ++ "|" ++ unwords b). words . dropWhile (not . isSpace) . snd) x
    writeFile "latinwords2.txt" (intercalate "\n\n" x)