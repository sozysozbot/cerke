module Main where

import Board
import GameState
import PrettyPrint

main :: IO ()
main = do
 b <- loadFile "test/board01.txt"
 let Just Fullboard{board = final, hand = pieces} = play3 b
 putStrLn $ drawBoard final
 print pieces



loadFile :: FilePath -> IO Board1
loadFile file = do
 str <- readFile file
 let Just b = loadBoard str
 return b

loadFile' :: [Char] -> IO ()
loadFile' file = do
 putStrLn "--------------------"
 putStrLn $ "Loading " ++ file
 str <- readFile file
 putStrLn $ str
 putStrLn ""
 print $ loadBoard str
 putStrLn ""
 putStrLn "Reverse:"
 let Just b = loadBoard str
 putStrLn $ drawBoard b