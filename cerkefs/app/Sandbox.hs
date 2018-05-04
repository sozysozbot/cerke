module Sandbox
(main
)
where
import CerkeFS


main :: IO ()
main = do
 print $ toDebugOutput $ do{vPlays3' sqKE  Tuk2 sqLE sqNE Downward; vPlays2' sqTAI Kauk2 sqTY Upward}
 foo "test" $ do
  vPlays2' sqTAI Kauk2 sqTY  >+> vPlays2' sqXI Kauk2 sqXU
  vPlays2' sqXAI Kauk2 sqXY  >+> vPlays2' sqZI Nuak1 sqZU
  vPlays2' sqZAI Nuak1 sqZY  >+> vPlays2  sqZO     sqXI
  vPlays2' sqXAU Dau2  sqZAI >+> vPlays3' sqXA Uai1 sqXE sqZI
  vPlays2' sqMAU Gua2  sqZAU >+> vPlays3' sqLE Gua2 sqTE sqZE
  vPlays3' sqPAU Tuk2  sqPAI sqPY  >+> vPlays3' sqXE Dau2 sqZI sqTU
  vPlays2' sqNAI Kauk2 sqNY  >+> vPlays3' sqTA Uai1  sqZE sqXE
  vPlays2' sqCAI Kauk2 sqCY  >+> vPlays2' sqXU Kauk2 sqXY
  vPlays2' sqZAI Dau2  sqXY  >+> vPlays2' sqTU Dau2  sqXY
  mun1(vPlays2' sqTAU Dau2  sqZAI) >+> vPlays3' sqME Gua2 sqMI sqMU
  vPlays2' sqMAI Kauk2 sqMY  >+> vPlays2' sqXY Dau2  sqPIA
  vPlays2' sqTAU Dau2  sqNAI >+> vPlays2' sqZU Nuak1 sqZY
  vPlays2' sqZAU Gua2  sqZY  >+> vPlays3' sqMU Gua2  sqMY sqMIA
  vPlays3' sqCIA Kaun1 sqXAU sqZAI >+> vPlays2' sqMIA Gua2 sqZIA
  declare Downward Dat2AIo 
  taxot1 


foo :: String -> Operation a2 -> IO ()
foo str fed = do
 putStrLn str
 putStrLn (toDebugOutput fed)
 print (toDebugOutput fed)









