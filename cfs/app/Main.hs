module Main where

import Lib
import Piece
import Board

main :: IO ()
main = do
 loadFile "test/board01.txt"
 --loadFile "test/board02.txt"
 --loadFile "test/board03.txt"

loadFile file = do
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