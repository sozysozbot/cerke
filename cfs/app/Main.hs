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
 plays Upward sqTAU sqTY
 plays Downward sqNI sqNU
 passes Upward
 plays Downward sqMA sqTO
 plays Upward sqTY sqTO
 plays Downward sqTA sqNI
 plays Upward sqLAU sqLO
 plays Downward sqZA sqZE
 drops Upward (Kok1, Maun1) sqKY
 passes Downward
 plays Upward sqKY sqZE 
 plays Downward sqXA sqZE
