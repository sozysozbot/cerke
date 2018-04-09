module Main where

import Board
import GameState
import PrettyPrint
import Control.Monad.Trans.State.Lazy
import Piece hiding(Piece(..))

main :: IO ()
main = do
 let Right Fullboard{board = final, hand = pieces} = playFromStart play2
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


play2 :: StateT Fullboard M ()
play2 = do
 movePieceFromTo_ (Square RAU CT) (Square RY CT)
 movePieceFromTo_ (Square RI CN)  (Square RU CN)
 movePieceFromTo_ (Square RA CM)  (Square RO CT)
 movePieceFromToTaking (Square RY CT)  (Square RO CT)
 movePieceFromTo_ (Square RA CT)  (Square RI CN)
 movePieceFromTo_ (Square RAU CL) (Square RO CL)
 movePieceFromTo_ (Square RA CZ)  (Square RE CZ)
 dropPiece (Kok1, Maun1, Upward) (Square RY CK)
 movePieceFromToTaking (Square RY CK) (Square RE CZ) 
 movePieceFromToTaking (Square RA CX)  (Square RE CZ)
