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
 foo "棋譜000:" fed000
 foo "エラー000:" err000
 foo "エラー001:" err001
 foo "エラー002:" err002
 foo "エラー003:" err003
 foo "エラー004:" err004

foo :: String -> StateT Fullboard M a2 -> IO ()
foo str fed = do
 putStrLn str
 case playFromStart fed of
  Left e -> putStrLn $ "error: " ++ show e
  Right Fullboard{board = final, hand = pieces} -> do 
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

err000, err001, err002, err003, err004 :: StateT Fullboard M ()
err000 = plays Downward sqTAU sqTY -- error: MovingOpponentPiece
err001 = plays Upward sqKAU sqLAU -- error: FriendlyFire
err002 = plays Upward sqNAU sqLAU -- error: EmptySquare sqNAU
err003 = drops Upward (Kok1, Maun1) sqKY -- error: NoCorrespondingPieceInHand
err004 = do -- error: TamCapture
 plays Upward sqZO sqCE
 plays Downward sqXA sqCE

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
