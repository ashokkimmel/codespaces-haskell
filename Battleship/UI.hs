import Text.Read (readMaybe)
import Update(fullupdate)
import Data.Maybe (fromMaybe)
import Data (UI(..),winner,pl1Name,pl2Name,setup,lookingatscreen,board,baseUI,Player(None))
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Arrow ((&&&))
import Data.Void
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Hoogle -- Personal convinience
import Control.Lens hiding (Control.Lens.Tutorial)
import GHC.IO

type Parser y = ParsecT Void String ((->) UI) y
linebrake = "-----------------------------------------\n"
coolfun :: (UI -> UI) -> ParsecT Void String (ReaderT UI IO)  UI
coolfun = lift . reader
type P = ParsecT Void String ((->) UI) (UI,IO ())
type Q = ParsecT Void String ((->) UI) (UI -> UI,String)
type Q' = ParsecT Void String ((->) UI) (UI,String)
qtoq' p = do 
 x <- ask 
 (a,b) <- p
 return (a x,b)
-- New type P = ParsecT Void String (ReaderT UI IO) UI
addUI :: Q -> P

ignore :: String -> ParsecT Void String x ()
ignore x = string' x *> space
getrest :: ParsecT Void String x String
getrest = some (satisfy (const True))
addUI p = do 
    x <- ask
    (a,b) <- p
    return (a x,putStrLn (linebrake ++b))

fullparse,parseset,pl1name,pl2name,addlines,parseadd,parseempty,unparsable,parsehelp :: P
fullparse = parseset <|> parseadd <|> parsehelp <|> unparsable <|> parseempty 
parseempty = addUI $ return (id,"")
parsehelp = ignore "help" *> (addUI . pure . (id,) . unsafePerformIO . readFile $ "Helpmessage.txt")
unparsable = addUI $ fmap (\x -> (id,"Your command, \"" ++ x ++ "\" had improper syntax")) getrest 
parsefail :: String -> P
parsefail y = addUI $ fmap (\x -> (id,"Your command, \"" ++ x ++ "\" was not a valid " ++ y ++" command.")) getrest 
parseset = ignore "set" *> (pl1name <|> pl2name <|> parsefail "set " <|> parseempty)
pl1name = addUI $ fmap (set pl1Name &&& ("Pl1 name is now: "++)) (ignore "pl1name" *> getrest)
pl2name = addUI $ fmap (set pl2Name &&& ("Pl2 name is now: "++)) (ignore "pl2name" *> getrest)

fromRead :: Maybe String -> String 
fromRead = fromMaybe "Invalid integer"
parseadd = ignore "add" *>  (addlines)-- <|> addattack) 
--addattack = ignore Attack 

addlines = addUI $ fmap (flip const &&& fromRead . fmap (unlines . flip replicate linebrake) . readMaybe) (ignore "lines" *> getrest)
main = do 
    traverse putStrLn ["Hello, welcome to battleship: type help to get all commands", "You are player 1", "Your boat length is 2, please enter it"]
    main' baseUI
main' :: UI -> IO ()
main' state =if (view (board.winner) state /= None) then print (view (board.winner) state) else do 
    x <- getLine 
    let (a,b) = parseresult x state 
    b
    main' a
parseresult str state = 
    case runParserT fullparse "" str $ state of Right (a,b) -> (a,b)
                                                Left x      -> (state,putStr $ errorBundlePretty x) 