module Main where

import Lib
import Piece
import Board
import StateT
import PrettyPrint

main :: IO ()
main = do
 b <- loadFile "test/board01.txt"
 let Just final = play b
 putStrLn $ drawBoard final



loadFile :: FilePath -> IO Board1
loadFile file = do
 str <- readFile file
 let Just b = loadBoard str
 return b

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