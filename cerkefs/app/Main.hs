module Main 
(module Main
)
where
import CerkeFS

main :: IO ()
main = do
 putStrLn "Hello, World!"

foo :: String -> Operation a2 -> IO ()
foo str fed = do
 putStrLn str
 putStrLn (toDebugOutput fed)
 print (toDebugOutput fed)



{-
loadFile :: FilePath -> IO Board1
loadFile file = do
 str <- readFile file
 let Just b = loadBoard str
 return b

loadFile' :: String -> IO ()
loadFile' file = do
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
-}






