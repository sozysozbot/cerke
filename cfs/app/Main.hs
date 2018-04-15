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
 foo "棋譜001:" fed001
 foo "エラー000:" err000
 foo "エラー001:" err001
 foo "エラー002:" err002
 foo "エラー003:" err003
 foo "エラー004:" err004
 foo "エラー005:" err005

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
err005 = do -- error: AmbiguousColor
 plays Upward sqTAI sqTY
 plays Downward sqTI sqTU
 plays Upward sqTY sqTU
 plays Downward sqNI sqNO
 plays Upward sqNAI sqNO
 plays Downward sqZA sqNE
 drops' Upward Kauk2 sqNAU
 

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

fed001 :: StateT Fullboard M ()
fed001 = do
 plays Upward sqTAU sqTY
 plays Downward sqXE sqXU
 plays Upward sqLIA sqXO
 passes Downward
 plays Upward sqZAI sqZY
 plays Downward sqME sqZE
 passes Upward
 plays Downward sqZO sqZAI
 plays Upward sqCAI sqCY
 passes Downward
 plays Upward sqXIA sqCAI
 passes Downward
 plays Upward sqCAI sqXY
 plays Downward sqXU sqTY
 plays Upward sqTAI sqTY
 plays Downward sqTE sqTU
 plays Upward sqXY sqCAU
 plays Downward sqTU sqMAU
 plays Upward sqCIA sqMAU
 plays Downward sqZAI sqXY
 plays Upward sqCAU sqCAI
 plays Downward sqZI sqZY
 plays Upward sqXY sqZAU
 plays Downward sqKE sqNE
 drops Upward (Huok2, Dau2) sqTAI
 plays Downward sqNI sqNU
 plays Upward sqXAI sqXY
 plays Downward sqNE sqNI
 plays Upward sqTY sqZY
 passes Downward
 passes Upward
 plays Downward sqNA sqXU
 plays Upward sqXO sqMI
 plays Downward sqCI sqMI
 -- [SY]為獣而手三 再行
 plays Upward sqXY sqXU
 plays Downward sqXI sqXU
 passes Upward
 plays Downward sqNI sqCI
 -- [SY]為行行而五 終季

