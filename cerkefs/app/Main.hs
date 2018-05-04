module Main
(main
)
where
import CerkeFS
import System.IO
import System.IO.Error
import PseudoStateT
import Control.Exception


type App = PseudoStateT Env

type Env = Maybe Fullboard

main :: IO ()
main = do
  guide
  runPseudoStateT bar' Nothing
  return ()

bar' :: App ()
bar' = do
 arr <- lift $ do
  putStr "CerkeFS> "
  hFlush stdout
  words <$> getLine
 case arr of
  ("quit":_) -> return ()
  ("exit":_) -> return ()
  [] -> bar'
  (x:xs) -> do
   command x xs
   bar'

command :: String -> [String] -> App ()
command "load_ascii" [] = lift $ putStrLn explain_load_ascii
command "load_ascii" (path:_) = do
 loadAsciiBoard2 path
command x _ = do
 lift $ putStrLn $ "Unknown command " ++ x ++ "."

guide :: IO ()
guide = putStr $ unlines 
 ["Guide:"
 ,"    quit: exits the application."
 ,"    exit: exits the application."
 ,explain_load_ascii
 ]

explain_load_ascii :: [Char]
explain_load_ascii = "    load_ascii FILENAME: loads the ASCII representation of the board."

loadAsciiBoard2 :: FilePath -> App ()
loadAsciiBoard2 file = pseudoStateT $ \orig -> do
 putStrLn "--------------------"
 putStrLn $ "Loading " ++ file
 str' <- tryJustReadFile file
 case str' >>= loadBoard' of
  Left err -> do 
   putStrLn $ "loading failed: " ++ err
   return ((),orig)
  Right b -> do
   putStrLn $ drawBoard b
   return ((),Just $ Fullboard{board = b, hand = []})


loadBoard' :: String -> Either String Board1
loadBoard' str = case loadBoard str of
 Just b -> Right b
 Nothing -> Left "incorrect format."

tryJustReadFile :: FilePath -> IO (Either String String)
tryJustReadFile filePath = tryJust handleReadFile (readFile filePath) where
 handleReadFile :: IOError -> Maybe String
 handleReadFile er
  | isDoesNotExistError er = Just "file does not exist."
  | isPermissionError   er = Just "permission denied."
  | otherwise              = Nothing


