module Main
(main
)
where
import CerkeFS
import System.IO
import PseudoStateT


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
 str <- readFile file
 case loadBoard str of
  Just b -> do
   putStrLn $ drawBoard b
   return ((),Just $ Fullboard{board = b, hand = []})
  Nothing -> do
   putStrLn "loading failed."
   return ((),orig)

