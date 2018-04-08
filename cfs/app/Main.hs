module Main where

import Lib
import Piece
import Board

main :: IO ()
main = do
 b <- loadFile "test/board01.txt"
 let Just final = play b
 putStrLn $ drawBoard final

play b = do
 b2 <- movePieceFromTo (Square RAU CT) (Square RY CT) b
 b3 <- movePieceFromTo (Square RI CN)  (Square RU CN) b2
 b4 <- movePieceFromTo (Square RA CM)  (Square RO CT) b3
 (horse,b5) <- removePiece     (Square RO CT) b4
 b5_ <- movePieceFromTo (Square RY CT)  (Square RO CT) b5
 b6 <- movePieceFromTo (Square RA CT)  (Square RI CN) b5_
 b7 <- movePieceFromTo (Square RAU CL)  (Square RO CL) b6
 b8 <- movePieceFromTo (Square RA CZ)  (Square RE CZ) b7
 b9 <- putPiece horse{side=Upward} (Square RY CK)  b8
 (king,b9_) <- removePiece     (Square RE CZ) b9
 b10 <- movePieceFromTo (Square RY CK) (Square RE CZ) b9_
 (horse2, b11) <- removePiece (Square RE CZ) b10
 b12 <- movePieceFromTo (Square RA CX)  (Square RE CZ) b11
 return b12

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