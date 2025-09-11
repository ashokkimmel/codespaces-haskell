import Control.Applicative (liftA3)
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
totuple :: [a] -> [(a,a)]
totuple (x:y:_:xs) = (x,y) : totuple xs
totuple _ = []

get1 :: [a] -> Maybe a
get1 (a:_) = Just a 
get1 _ = Nothing
get2 :: [a] -> Maybe a
get2 (_:b:_) = Just b
get2 _ = Nothing
get3 :: [a] -> Maybe a
get3 (_:_:c:_) = Just c
get3 _ = Nothing
getall :: [[a]] -> ([a],[a],[a])
getall = liftA3 (,,) (mapMaybe get1) (mapMaybe get2) (mapMaybe get3)
merge3 (a,b,c) = a ++ b ++ c
--parsebase64table :: String -> [(String,String)]
printread :: (String,String) -> String 
printread (a,b) = "    ReadNat '" ++ a ++ "' = " ++ b
printshow (a,b) = "    ShowNat " ++ b ++ " = '" ++ a ++ "'"
parsebase64table :: String -> IO ()
parsebase64table =  writeFile "families" . unlines . uncurry (<>) . (map printread &&& map printshow) . merge3 . getall . map (totuple . words) . lines 