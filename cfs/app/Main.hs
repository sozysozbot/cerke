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


f >+> g = f Upward >> g Downward

err000, err001, err002, err003, err004 :: StateT Fullboard M ()
err000 = plays sqTAU sqTY Downward -- error: MovingOpponentPiece
err001 = plays sqKAU sqLAU Upward -- error: FriendlyFire
err002 = plays sqNAU sqLAU Upward -- error: EmptySquare sqNAU
err003 = drops (Kok1, Maun1) sqKY Upward -- error: NoCorrespondingPieceInHand
err004 = plays sqZO sqCE >+> plays sqXA sqCE -- error: TamCapture
err005 = do -- error: AmbiguousColor
 plays sqTAI sqTY >+> plays sqTI sqTU
 plays sqTY  sqTU >+> plays sqNI sqNO
 plays sqNAI sqNO >+> plays sqZA sqNE
 drops' Kauk2 sqNAU Upward
 

fed000 :: StateT Fullboard M ()
fed000 = do
 plays sqTAU sqTY >+> plays sqNI sqNU
 passes           >+> plays sqMA sqTO
 plays sqTY sqTO  >+> plays sqTA sqNI
 plays sqLAU sqLO >+> plays sqZA sqZE
 drops (Kok1, Maun1) sqKY >+> passes
 plays sqKY  sqZE >+> plays sqXA sqZE

fed001 :: StateT Fullboard M ()
fed001 = do
 plays sqTAU sqTY  >+> plays sqXE sqXU
 plays sqLIA sqXO  >+> passes
 plays sqZAI sqZY  >+> plays sqME sqZE
 passes            >+> plays sqZO sqZAI
 plays sqCAI sqCY  >+> passes 
 plays sqXIA sqCAI >+> passes
 plays sqCAI sqXY  >+> plays sqXU sqTY
 plays sqTAI sqTY  >+> plays sqTE sqTU
 plays sqXY  sqCAU >+> plays sqTU sqMAU
 plays sqCIA sqMAU >+> plays sqZAI sqXY
 plays sqCAU sqCAI >+> plays sqZI sqZY
 plays sqXY sqZAU  >+> plays sqKE sqNE
 drops (Huok2, Dau2) sqTAI >+> plays sqNI sqNU
 plays sqXAI sqXY  >+> plays sqNE sqNI
 plays sqTY  sqZY  >+> passes
 passes            >+> plays sqNA sqXU
 plays sqXO sqMI   >+> plays sqCI sqMI
 -- [SY]為獣而手三 再行
 plays sqXY sqXU   >+> plays sqXI sqXU
 passes            >+> plays sqNI sqCI
 -- [SY]為行行而五 終季

