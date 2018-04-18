module Main 
(module Main
)
where
import CerkeFS

main :: IO ()
main = do
 foo "棋譜000:" fed000
 foo "棋譜001:" fed001
 foo "棋譜002:" fed002
 foo "棋譜003:" fed003
 foo "棋譜004:" fed004
 foo "棋譜005:" fed005

foo :: String -> Operation a2 -> IO ()
foo str fed = do
 putStrLn str
 case playFromStart fed of
  Left e -> putStrLn $ "error: " ++ show e
  Right Fullboard{board = final, hand = pieces} -> do 
   putStr $ drawBoard final
   putStrLn "~~~"
   putStrLn $ concatMap convertPieceToStr pieces
   putStrLn ""



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


fed000 :: Operation ()
fed000 = do
 plays'      sqTAU 虎 sqTY  >+> plays' sqNI 兵 sqNU
 mun1(plays' sqLIA 馬 sqXO) >+> plays' sqMA 馬 sqTO
 plays'      sqTY  虎 sqTO  >+> plays' sqTA 将 sqNI
 plays'      sqLAU 弓 sqLO  >+> plays' sqZA 王 sqZE
 drops'            馬 sqKY  >+> mun1(plays' sqNU 兵 sqNO)
 plays'      sqKY  馬 sqZE  >+> plays' sqXA 将 sqZE

fed001 :: Operation ()
fed001 = do
 plays'      sqTAU 虎 sqTY  >+> plays'      sqXE 虎 sqXU
 plays'      sqLIA 馬 sqXO  >+> mun1(plays' sqXI 兵 sqXO)
 plays'      sqZAI 船 sqZY  >+> plays'      sqME 弓 sqZE
 mun1(plays' sqXIA 将 sqZAI)>+> playsTam    sqZO    sqZAI
 plays'      sqCAI 兵 sqCY  >+> passes 
 plays'      sqXIA 将 sqCAI >+> passes
 plays'      sqCAI 将 sqXY  >+> plays'      sqXU 虎 sqTY
 plays'      sqTAI 兵 sqTY  >+> plays'      sqTE 虎 sqTU
 plays'      sqXY  将 sqCAU >+> plays'      sqTU 虎 sqMAU
 plays'      sqCIA 車 sqMAU >+> playsTam    sqZAI   sqXY
 plays'      sqCAU 将 sqCAI >+> plays'      sqZI 船 sqZY
 playsTam    sqXY     sqZAU >+> plays'      sqKE 巫 sqNE
 drops        (黒, 虎) sqTAI >+> plays'      sqNI 兵 sqNU
 plays'      sqXAI 兵 sqXY  >+> plays'      sqNE 巫 sqNI
 plays'      sqTY  兵 sqZY  >+> mun1(plays' sqZE 弓 sqZY)
 mun1(plays' sqNIA 車 sqZAI)>+> plays'      sqNA 車 sqXU
 plays'      sqXO  馬 sqMI  >+> plays'      sqCI 兵 sqMI
 declare Downward Saup1
 plays'      sqXY  兵 sqXU  >+> plays'      sqXI 兵 sqXU
 drops'            車 sqCI  >+> plays'      sqNI 巫 sqCI
 declare Downward Mok1Mok1
 taxot1



fed002 :: Operation ()
fed002 = do
 plays' sqKE  Tuk2  sqNE >-> plays' sqTAI Kauk2 sqTY
 plays' sqNI  Kauk2 sqNU >-> playsTam sqZO        sqZAU
 plays' sqNE  Tuk2  sqNI >-> plays' sqNAI Kauk2 sqNY
 plays' sqNI  Tuk2  sqTU >-> plays' sqKAU Tuk2  sqNAU
 plays' sqTA  Uai1  sqNI >-> plays' sqXAI Kauk2 sqXY
 plays' sqTE  Dau2  sqLU >-> plays' sqNAU Tuk2 sqNAI
 plays' sqXE  Dau2  sqXU >-> plays' sqCAI Kauk2 sqCY
 plays' sqPE  Tuk2  sqCE >-> plays' sqXIA Uai1  sqXAI
 plays' sqCI  Kauk2 sqCU >-> plays' sqTIA Uai1  sqTAI
 plays' sqXA  Uai1  sqCI >-> plays' sqTAU Dau2  sqLY
 plays' sqMA  Maun1 sqTO >-> plays' sqTY  Kauk2 sqTO
 playsTam sqZAU       sqZO >-> playsTam sqZO        sqZY
 plays' sqLE  Gua2  sqZE >-> plays' sqZAI Nuak1 sqCAI
 plays' sqLA  Maun1 sqLO >-> mun1 (plays' sqXAI Uai1 sqZAI)
 plays' sqKA  Kua2  sqKU >-> plays' sqLAI Kauk2 sqLO
 plays' sqLI  Kauk2 sqLO >-> plays' sqLY  Dau2  sqTY
 mun1 (plays' sqZE Gua2 sqZU) >-> plays' sqZIA Io sqCAU
 playsTam sqZY        sqCO >-> playsTam sqCO        sqZAU
 plays' sqZI  Nuak1 sqZO >-> plays' sqTY Dau2   sqZO
 mun1 (plays' sqZE Gua2 sqZO) >-> plays' sqZO Dau2 sqNI
 plays' sqTU  Tuk2  sqNI >-> plays' sqTAI Uai1  sqLO
 plays' sqLU  Dau2  sqTE >-> plays' sqNAI Tuk2  sqCI
 plays' sqCE  Tuk2  sqCI >-> plays' sqXY  Kauk2 sqXU
 declare Upward Saup1
 taxot1

fed003 :: Operation ()
fed003 = do
 plays'   sqXIA Uai1  sqZAU >+> plays' sqTI Kauk2 sqTU
 playsTam sqZO        sqCY  >+> plays' sqXI Kauk2 sqXU
 plays'   sqMAI Kauk2 sqMY  >+> plays' sqNI Kauk2 sqNU
 playsTam sqCY        sqCAU >+> plays' sqKE Tuk2  sqNE
 plays'   sqCAI Kauk2 sqCY  >+> plays' sqNE Tuk2  sqNI
 plays'   sqZAI Nuak1 sqZY  >+> plays' sqZI Nuak1 sqZY
 plays'   sqZAU Uai1  sqZY  >+> plays' sqLE Gua2  sqZE
 plays'   sqZY  Uai1  sqCAI >+> plays' sqZE Gua2  sqZIA
 declare Downward Dat2AIo
 taxot1

fed004 :: Operation ()
fed004 = do
 plays' sqTAI Kauk2 sqTY  >+> plays' sqXI Kauk2 sqXU
 plays' sqXAI Kauk2 sqXY  >+> plays' sqZI Nuak1 sqZU
 plays' sqZAI Nuak1 sqZY  >+> playsTam sqZO     sqXI
 plays' sqXAU Dau2  sqZAI >+> plays' sqXA Uai1  sqZI
 plays' sqMAU Gua2  sqZAU >+> plays' sqLE Gua2  sqZE
 plays' sqPAU Tuk2  sqPY  >+> plays' sqXE Dau2  sqTU
 plays' sqNAI Kauk2 sqNY  >+> plays' sqTA Uai1  sqXE
 plays' sqCAI Kauk2 sqCY  >+> plays' sqXU Kauk2 sqXY
 plays' sqZAI Dau2  sqXY  >+> plays' sqTU Dau2  sqXY
 mun1(plays' sqTAU Dau2  sqZAI) >+> plays' sqME Gua2  sqMU
 plays' sqMAI Kauk2 sqMY  >+> plays' sqXY Dau2  sqPIA
 plays' sqTAU Dau2  sqNAI >+> plays' sqZU Nuak1 sqZY
 plays' sqZAU Gua2  sqZY  >+> plays' sqMU Gua2  sqMIA
 plays' sqCIA Kaun1 sqZAI >+> plays' sqMIA Gua2 sqZIA
 declare Downward Dat2AIo
 taxot1

fed005 :: Operation ()
fed005 = do
 plays' sqTE 虎 sqTU  >-> plays' sqTAI 兵 sqTY
 plays' sqZI 船 sqZU  >-> plays' sqTAU 虎 sqXY
 playsTam sqZO  sqCE  >-> plays' sqXIA 将 sqZAU
 plays' sqLE 弓 sqZE  >-> plays' sqTY  兵 sqTU
 plays' sqTI 兵 sqTU  >-> plays' sqZIA 王 sqXIA
 plays' sqME 弓 sqMU  >-> plays' sqXY  虎 sqMU
 plays' sqPE 巫 sqME  >-> plays' sqLIA 馬 sqTAI
 plays' sqMI 兵 sqMU  >-> plays' sqLAU 弓 sqLY
 plays' sqZE 弓 sqTE  >-> mun1(plays' sqLY  弓 sqZY)
 plays' sqTU 兵 sqTY  >-> plays' sqZAI 船 sqZU
 plays' sqTY 兵 sqTAI Downward
 declare Downward Saup1
 taxot1
