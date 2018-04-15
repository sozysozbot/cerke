module Main 
(module Main
)
where

import Board
import GameState
import PrettyPrint
import Control.Monad.Trans.State.Lazy
import Piece3

main :: IO ()
main = do
 foo "棋譜000:" fed000
 foo "棋譜001:" fed001
 foo "棋譜002:" fed002
 foo "棋譜003:" fed003
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

(>+>), (>->) :: Monad m => (Side -> m a) -> (Side -> m b) -> m b
f >+> g = f Upward >> g Downward
f >-> g = f Downward >> g Upward

err000, err001, err002, err003, err004, err005 :: StateT Fullboard M ()
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
 plays'      sqTAU 虎 sqTY  >+> plays' sqNI 兵 sqNU
 mun1(plays' sqLIA 馬 sqXO) >+> plays' sqMA 馬 sqTO
 plays'      sqTY  虎 sqTO  >+> plays' sqTA 将 sqNI
 plays'      sqLAU 弓 sqLO  >+> plays' sqZA 王 sqZE
 drops'            馬 sqKY  >+> mun1(plays' sqNU 兵 sqNO)
 plays'      sqKY  馬 sqZE  >+> plays' sqXA 将 sqZE

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



fed002 :: StateT Fullboard M ()
fed002 = do
 plays' sqKE  Tuk2  sqNE >-> plays' sqTAI Kauk2 sqTY
 plays' sqNI  Kauk2 sqNU >-> playsT sqZO        sqZAU
 plays' sqNE  Tuk2  sqNI >-> plays' sqNAI Kauk2 sqNY
 plays' sqNI  Tuk2  sqTU >-> plays' sqKAU Tuk2  sqNAU
 plays' sqTA  Uai1  sqNI >-> plays' sqXAI Kauk2 sqXY
 plays' sqTE  Dau2  sqLU >-> plays' sqNAU Tuk2 sqNAI
 plays' sqXE  Dau2  sqXU >-> plays' sqCAI Kauk2 sqCY
 plays' sqPE  Tuk2  sqCE >-> plays' sqXIA Uai1  sqXAI
 plays' sqCI  Kauk2 sqCU >-> plays' sqTIA Uai1  sqTAI
 plays' sqXA  Uai1  sqCI >-> plays' sqTAU Dau2  sqLY
 plays' sqMA  Maun1 sqTO >-> plays' sqTY  Kauk2 sqTO
 playsT sqZAU       sqZO >-> playsT sqZO        sqZY
 plays' sqLE  Kaun1 sqZE >-> plays' sqZAI Nuak1 sqCAI
 plays' sqLA  Maun1 sqLO >-> mun1 (plays' sqXAI Uai1 sqZAI)
 plays' sqKA  Kua2  sqKU >-> plays' sqLAI Kauk2 sqLO
 plays' sqLI  Kauk2 sqLO >-> plays' sqLY  Dau2  sqTY
 mun1 (plays' sqZE Gua2 sqZU) >-> plays' sqZIA Io sqCAU
 playsT sqZY        sqCO >-> playsT sqCO        sqZAU
 plays' sqZI  Nuak1 sqZO >-> plays' sqTY Dau2   sqZO
 mun1 (plays' sqZE Gua2 sqZO) >-> plays' sqZO Dau2 sqNI
 plays' sqTU  Tuk2  sqNI >-> plays' sqTAI Uai1  sqLO
 plays' sqLU  Dau2  sqTE >-> plays' sqNAI Tuk2  sqCI
 plays' sqCE  Tuk2  sqCI >-> plays' sqXY  Kauk2 sqXU
 -- [JV] zau saup1 ua hop1 om2.
 -- ta xot1.

fed003 :: StateT Fullboard M ()
fed003 = do
 plays' sqXIA Uai1  sqZAU >+> plays' sqTI Kauk2 sqTU
 playsT sqZO        sqCY  >+> plays' sqXI Kauk2 sqXU
 plays' sqMAI Kauk2 sqMY  >+> plays' sqNI Kauk2 sqNU
 playsT sqCY        sqCAU >+> plays' sqKE Tuk2  sqNE
 plays' sqCAI Kauk2 sqCY  >+> plays' sqNE Tuk2  sqNI
 plays' sqZAI Nuak1 sqZY  >+> plays' sqZI Nuak1 sqZY
 plays' sqZAU Uai1  sqZY  >+> plays' sqLE Gua2  sqZE
 plays' sqZY  Uai1  sqCAI >+> plays' sqZE Gua2  sqZIA
 --[j.v] zau io hop1 om2.
 -- ta xot1.
