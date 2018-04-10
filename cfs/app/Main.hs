module Main 
(module Main
)
where

import Board
import GameState
import PrettyPrint
import Control.Monad.Trans.State.Lazy
import Piece hiding(Piece(..))

main :: IO ()
main = do
 putStrLn "棋譜000:"
 let Right Fullboard{board = final, hand = pieces} = playFromStart fed000
 putStrLn $ drawBoard final
 print pieces



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


fed000 :: StateT Fullboard M ()
fed000 = do
 movePieceFromTo2 sqTAU sqTY
 movePieceFromTo2 sqNI sqNU
 pass
 movePieceFromTo2 sqMA sqTO
 movePieceFromTo2 sqTY sqTO
 movePieceFromTo2 sqTA sqNI
 movePieceFromTo2 sqLAU sqLO
 movePieceFromTo2 sqZA sqZE
 dropPiece (Kok1, Maun1, Upward) sqKY
 pass
 movePieceFromTo2 sqKY sqZE 
 movePieceFromTo2 sqXA sqZE
