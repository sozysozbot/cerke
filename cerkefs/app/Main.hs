module Main
(main
)
where
import CerkeFS
import System.IO

main :: IO ()
main = do
  guide
  bar

bar :: IO ()
bar = do
 --hSetBuffering stdout NoBuffering
 putStr "CerkeFS> "
 hFlush stdout
 arr <- words <$> getLine
 case arr of
  ("quit":_) -> return ()
  ("exit":_) -> return ()
  [] -> bar
  (x:xs) -> do
   command x xs
   bar

command :: String -> [String] -> IO ()
command "load_ascii" [] = putStrLn explain_load_ascii
command "load_ascii" (path:_) = do
 loadAsciiBoard' path
command x xs = do
  putStrLn $ "Unknown command " ++ x ++ "."

guide :: IO ()
guide = putStr $ unlines 
 ["Guide:"
 ,"    quit: exits the application."
 ,"    exit: exits the application."
 ,explain_load_ascii
 ]

explain_load_ascii = "    load_ascii FILENAME: loads the ASCII representation of the board."

loadAsciiBoard :: FilePath -> IO Board1
loadAsciiBoard file = do
 str <- readFile file
 let Just b = loadBoard str
 return b

loadAsciiBoard' :: String -> IO ()
loadAsciiBoard' file = do
 putStrLn "--------------------"
 putStrLn $ "Loading " ++ file
 str <- readFile file
 putStrLn str
 putStrLn ""
 print $ loadBoard str
 putStrLn ""
 putStrLn "Reverse:"
 let Just b = loadBoard str
 putStrLn $ drawBoard b
