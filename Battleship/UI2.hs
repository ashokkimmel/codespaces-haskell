import Text.Read (readMaybe)
import Control.Monad (guard,join)
import Data.Functor (($>))
import Update(fullupdate,anychange)
import Data.Maybe (fromMaybe)
import Data (UI(..),winner,pl1Name,pl2Name,setup,lookingatscreen,board,baseUI,Player(None))
import Text.Megaparsec
import Data.Function (on)
import Text.Megaparsec.Char(string',digitChar,space)
import Control.Arrow ((&&&))
import Data.Void(Void)
import Control.Monad.Reader(ReaderT,ask,runReaderT,lift)
import Hoogle(hoogle) -- Personal convinience
import Control.Lens(view,over,set)
import GHC.IO(unsafePerformIO)
linebrake :: String 
linebrake = "-----------------------------------------\n"
type P = ParsecT Void String (ReaderT UI IO) UI
type Q = ParsecT Void String (ReaderT UI IO) (UI -> UI,String)
addUI :: Q -> P
addUI p = do 
    x <- ask
    (a,b) <- p
    llift (putStrLn (linebrake ++ b))
    return (a x)
llift :: IO a -> ParsecT Void String (ReaderT UI IO) a
llift = lift . lift 
ignore :: String -> ParsecT Void String x ()
ignore x = string' x *> space
getrest :: ParsecT Void String x String
getrest = some (satisfy (const True))
-- addUI = undefined
addnolineUI ::  Q -> P
addnolineUI p = do 
    x <- ask
    (a,_) <- p
    return (a x)
fullparse,parseset,pl1name,pl2name,addlines,parseadd,parseempty,unparsable,parsehelp,addattack,parseattack :: P
fullparse = parseset <|> parseadd <|> parsehelp <|> unparsable <|> parseempty 
parseempty = addnolineUI $ return (id,"")
parsehelp = ignore "help" *> (addUI . pure . (id,) . unsafePerformIO . readFile $ "Helpmessage.txt")
unparsable = addUI $ fmap (\x -> (id,"Your command, \"" ++ x ++ "\" had improper syntax")) getrest 
parsefail :: String -> P
parsefail y = addUI $ fmap (\x -> (id,"Your command, \"" ++ x ++ "\" was not a valid " ++ y ++ " command.")) getrest 
parseset = ignore "set" *> (pl1name <|> pl2name <|> parsefail "set " <|> parseempty)
pl1name = addUI $ fmap (set pl1Name &&& ("Pl1 name is now: " ++)) (ignore "pl1name" *> getrest)
pl2name = addUI $ fmap (set pl2Name &&& ("Pl2 name is now: " ++)) (ignore "pl2name" *> getrest)
fromRead :: Maybe String -> String 
fromRead = fromMaybe "Invalid integer"
parseadd = ignore "add" *>  (addlines <|> addattack) 
addattack = ignore "Attack" *> do 
    llift $ putStr "Do you want to attack? y/n: "
    confirm parseattack "attack"

confirm x str = do 
    confirmation <- llift getChar
    if elem confirmation "yY" then x else (if (elem confirmation "nN") then llift (putStrLn ("\nYou declined to " ++ str)) *> ask else llift (putStr "\nThat was an invalid character, please type a new one: ") *> confirm x str)
parseattack = do 
    (x,y) <- parsetuple
    state <- ask 
    let c = (over board (fullupdate (x,y)) state)
    if (anychange `on` (view board)) c state then llift $ putStrLn "\nYour attack was a hit" else llift $ putStrLn "\nYour attack was a miss"
    return c 

addlines = addUI $ fmap (flip const &&& fromRead . fmap (unlines . flip replicate linebrake) . readMaybe) (ignore "lines" *> getrest)
main :: IO ()
main = do 
    fmap mconcat $ traverse putStrLn ["Hello, welcome to battleship: type help to get all commands", "You are player 1", "Your boat length is 2, please enter it"]
    main' baseUI
main' :: UI -> IO ()
main' state = print (view board state) *> if (view (board.winner) state /= None) then putStrLn (show (view (board.winner) state)++ " WON!")  else do 
    x <- getLine 
    a <- parseresult x state 
    main' a
parseresult :: String -> UI -> (IO UI)
parseresult str state =  parseresulth state (runReaderT (runParserT fullparse "" str) state)
parseresulth :: UI -> IO (Either (ParseErrorBundle String Void) UI) -> IO UI 
parseresulth state = (parseresulthioless =<<) where 
    parseresulthioless (Left x) = putStr (errorBundlePretty x) *> return state 
    parseresulthioless (Right a) = return a 
-- switchscreen = ignore "switch" *> ignore "screen" *> do 
parsetuple :: ParsecT Void String (ReaderT UI IO) (Int,Int)
parsetuple = do 
    ignore "(" 
    x <- some digitChar 
    ignore ","
    y <- some digitChar 
    ignore ")"
    return (read x,read y)
