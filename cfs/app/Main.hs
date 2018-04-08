module Main where

import Lib
import Piece
import Board

main :: IO ()
main = do
 str <- readFile "test/board01.txt"
 putStrLn $ str
 putStrLn ""
 print $ loadBoard str