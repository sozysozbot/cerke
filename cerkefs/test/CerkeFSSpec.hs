module CerkeFSSpec (spec) where

import Test.Hspec
import CerkeFS
import CerkeFS.TestUtilities

spec :: Spec
spec = do
  describe "error detections" $ do
    it "privilege exceeded by Kauk2" $
      ProfessionPrivilegeExceeded Kauk2 sqTI `shouldBeThrownBy` vPlays2 sqTI sqNE Downward
    it "privilege exceeded by Kauk2" $
      ProfessionPrivilegeExceeded Kauk2 sqTI `shouldBeThrownBy` vPlays2 sqTI sqTO Downward
    it "privilege exceeded by Kauk2" $
      ProfessionPrivilegeExceeded Kauk2 sqKAI `shouldBeThrownBy` vPlays2 sqKAI sqZAU Upward
    it "privilege exceeded by Kauk2, but MovingOpponentPiece takes precedence" $
      MovingOpponentPiece `shouldBeThrownBy` vPlays2 sqKAI sqZAU Downward
    it "Tam2 captures a piece (539_automatic)" $
      CaptureByTam `shouldBeThrownBy` (vPlays3 sqZO sqZO sqZU >+> vPlays3 sqZU sqZI sqZA)
    it "Tam2 captures a piece (079_automatic)" $
      CaptureByTam `shouldBeThrownBy` do
       vPlays3 sqZO sqCI sqMU >+> vPlays2 sqMU sqCU
       vPlays3 sqCU sqCI sqMA >+> vPlays3 sqME sqPI sqPY
       vPlays3 sqMA sqXA sqZA Upward
  describe "correct behaviors" $ do
    it "sample in the haddock" $ "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n - _2k_7k_4k - _4h - _2h_7h\n_1h_1k_1h_1k_!k_1k_1h_1k_1h\n -  -  -  -  -  -  -  -  - \n -  -  -  - ^$h -  -  -  - \n -  -  - ^1k -  -  -  -  - \n^1h^1k^1h - ^!h^1k^1h^1k^1h\n^7h^2h - ^4h - ^4k - ^2k^7k\n^6k^5k^3k^8k^#h^8h^3h^5h^6h\n~~~\n\n" `shouldBeGeneratedBy` do{vPlays3' sqKE  Tuk2 sqLE sqNE Downward; vPlays2' sqTAI Kauk2 sqTY Upward}
    it "fedirrgavir 001, Tam2 also validated" $ res001 `shouldBeGeneratedBy` fed001''
    it "fedirrgavir 002, Tam2 also validated" $ res002 `shouldBeGeneratedBy` fed002''
    it "fedirrgavir 003, Tam2 also validated" $ res003 `shouldBeGeneratedBy` fed003''
    it "fedirrgavir 004, Tam2 also validated" $ res004 `shouldBeGeneratedBy` fed004''
    it "fedirrgavir 005, Tam2 also validated" $ res005 `shouldBeGeneratedBy` fed005''
    it "fedirrgavir 003, Tam2 uses vPlays2"   $ res003 `shouldBeGeneratedBy` fed003'''
    it "fedirrgavir 005, Tam2 uses vPlays3"   $ res005 `shouldBeGeneratedBy` fed005'''
    it "random example" $ str `shouldBeGeneratedBy` do
     vPlays3 sqZO sqTI sqTU >-> vPlays3 sqTAU sqNAI sqTY
     vPlays3 sqNI sqLI sqLU >-> vPlays3 sqLAU sqKAU sqTAU
     vPlays3 sqZI sqZAI sqZIA Downward
  describe "searching hands" $ do
    it "initialFullBoard, Downward" $ 
      (testAll Downward initialFullBoard) `shouldBeSimilarTo` cand000
    it "After fedirrgavir 001" $
      let Right board = playFromStart fed001'' in (testAll Upward board) `shouldBeSimilarTo` cand001

cand000 = [Move2 sqTA sqNE,Move2 sqTA sqZE,Move2 sqZA sqZE,Move2 sqXA sqZE,Move2 sqXA sqCE,Move2 sqLE sqNE,Move2 sqME sqCE,Move2 sqKI sqKU,Move2 sqLI sqLU,Move2 sqNI sqNE,Move2 sqNI sqNU,Move2 sqNI sqNO,Move2 sqTI sqTU,Move2 sqZI sqZU,Move2 sqXI sqXU,Move2 sqCI sqCE,Move2 sqCI sqCU,Move2 sqCI sqCO,Move2 sqMI sqMU,Move2 sqPI sqPU,Move2 sqZO sqNU,Move2 sqZO sqTU,Move2 sqZO sqZU,Move2 sqZO sqXU,Move2 sqZO sqCU,Move2 sqZO sqNO,Move2 sqZO sqTO,Move2 sqZO sqXO,Move2 sqZO sqCO,Move2 sqZO sqNY,Move2 sqZO sqTY,Move2 sqZO sqZY,Move2 sqZO sqXY,Move2 sqZO sqCY,Move3 sqLA sqTI sqLO,Move3 sqLA sqTI sqXO,Move3 sqTA sqNA sqNE,Move3 sqTA sqZA sqZE,Move3 sqTA sqTE sqNE,Move3 sqTA sqTE sqZE,Move3 sqZA sqTA sqNE,Move3 sqZA sqTA sqZE,Move3 sqZA sqXA sqZE,Move3 sqZA sqXA sqCE,Move3 sqZA sqTE sqNE,Move3 sqZA sqTE sqZE,Move3 sqZA sqXE sqZE,Move3 sqZA sqXE sqCE,Move3 sqXA sqZA sqZE,Move3 sqXA sqCA sqCE,Move3 sqXA sqXE sqZE,Move3 sqXA sqXE sqCE,Move3 sqMA sqXI sqTO,Move3 sqMA sqXI sqMO,Move3 sqKE sqLE sqNE,Move3 sqKE sqKI sqKU,Move3 sqLE sqKE sqNE,Move3 sqLE sqTE sqZE,Move3 sqLE sqLI sqLU,Move3 sqLE sqLI sqLO,Move3 sqLE sqLI sqLY,Move3 sqLE sqLI sqLAI,Move3 sqTE sqNI sqLU,Move3 sqTE sqNI sqTU,Move3 sqTE sqNI sqKO,Move3 sqTE sqZI sqTU,Move3 sqTE sqZI sqXU,Move3 sqXE sqZI sqTU,Move3 sqXE sqZI sqXU,Move3 sqXE sqCI sqXU,Move3 sqXE sqCI sqMU,Move3 sqXE sqCI sqPO,Move3 sqME sqXE sqZE,Move3 sqME sqPE sqCE,Move3 sqME sqMI sqMU,Move3 sqME sqMI sqMO,Move3 sqME sqMI sqMY,Move3 sqME sqMI sqMAI,Move3 sqPE sqME sqCE,Move3 sqPE sqPI sqPU,Move3 sqNI sqLI sqLU,Move3 sqNI sqTI sqTU,Move3 sqZI sqZO sqZE,Move3 sqZI sqZO sqNO,Move3 sqZI sqZO sqTO,Move3 sqZI sqZO sqXO,Move3 sqZI sqZO sqCO,Move3 sqZI sqZO sqZY,Move3 sqZI sqZO sqZAI,Move3 sqCI sqXI sqXU,Move3 sqCI sqMI sqMU,Move3 sqZO sqNI sqNE,Move3 sqZO sqNI sqLU,Move3 sqZO sqNI sqNU,Move3 sqZO sqTI sqNE,Move3 sqZO sqTI sqZE,Move3 sqZO sqTI sqNU,Move3 sqZO sqTI sqTU,Move3 sqZO sqTI sqZU,Move3 sqZO sqZI sqZE,Move3 sqZO sqZI sqTU,Move3 sqZO sqZI sqZU,Move3 sqZO sqZI sqXU,Move3 sqZO sqXI sqZE,Move3 sqZO sqXI sqCE,Move3 sqZO sqXI sqZU,Move3 sqZO sqXI sqXU,Move3 sqZO sqXI sqCU,Move3 sqZO sqCI sqCE,Move3 sqZO sqCI sqCU,Move3 sqZO sqCI sqMU,Move3 sqZO sqZO sqTU,Move3 sqZO sqZO sqZU,Move3 sqZO sqZO sqXU,Move3 sqZO sqZO sqTO,Move3 sqZO sqZO sqXO,Move3 sqZO sqZO sqTY,Move3 sqZO sqZO sqZY,Move3 sqZO sqZO sqXY,Move3 sqZO sqNAI sqLY,Move3 sqZO sqNAI sqNY,Move3 sqZO sqNAI sqNAU,Move3 sqZO sqTAI sqNY,Move3 sqZO sqTAI sqTY,Move3 sqZO sqTAI sqZY,Move3 sqZO sqTAI sqNAU,Move3 sqZO sqTAI sqZAU,Move3 sqZO sqZAI sqTY,Move3 sqZO sqZAI sqZY,Move3 sqZO sqZAI sqXY,Move3 sqZO sqZAI sqZAU,Move3 sqZO sqXAI sqZY,Move3 sqZO sqXAI sqXY,Move3 sqZO sqXAI sqCY,Move3 sqZO sqXAI sqZAU,Move3 sqZO sqXAI sqCAU,Move3 sqZO sqCAI sqCY,Move3 sqZO sqCAI sqMY,Move3 sqZO sqCAI sqCAU]

cand001 = [Move2 sqZY sqZO,Move2 sqCY sqCO,Move2 sqKAI sqKY,Move2 sqLAI sqLY,Move2 sqNAI sqNO,Move2 sqNAI sqNY,Move2 sqNAI sqNAU,Move2 sqTAI sqKU,Move2 sqTAI sqLO,Move2 sqTAI sqNY,Move2 sqTAI sqNAU,Move2 sqTAI sqLIA,Move2 sqCAI sqXY,Move2 sqCAI sqMY,Move2 sqCAI sqXAI,Move2 sqCAI sqCAU,Move2 sqMAI sqMY,Move2 sqPAI sqPY,Move2 sqLAU sqNAU,Move2 sqLAU sqTAU,Move2 sqLAU sqLIA,Move2 sqZAU sqTY,Move2 sqZAU sqXY,Move2 sqZAU sqZAI,Move2 sqZAU sqXAI,Move2 sqZAU sqNAU,Move2 sqZAU sqTAU,Move2 sqZAU sqCAU,Move2 sqZAU sqCIA,Move2 sqXAU sqKI,Move2 sqXAU sqLU,Move2 sqXAU sqNO,Move2 sqXAU sqTY,Move2 sqXAU sqZAI,Move2 sqXAU sqCIA,Move2 sqMAU sqCIA,Move2 sqKIA sqLIA,Move2 sqNIA sqPI,Move2 sqNIA sqMU,Move2 sqNIA sqCO,Move2 sqNIA sqXY,Move2 sqNIA sqZAI,Move2 sqNIA sqTAU,Move2 sqTIA sqNAU,Move2 sqTIA sqTAU,Move2 sqZIA sqTAU,Move2 sqZIA sqXIA,Move2 sqMIA sqXAI,Move3 sqNAI sqLAI sqLY,Move3 sqNAI sqTAI sqTO,Move3 sqNAI sqTAI sqTY,Move3 sqNAI sqTAI sqZAI,Move3 sqNAI sqTAI sqTAU,Move3 sqTAI sqZY sqTO,Move3 sqTAI sqZY sqXO,Move3 sqTAI sqZY sqXAI,Move3 sqTAI sqZAU sqKU,Move3 sqTAI sqZAU sqLO,Move3 sqTAI sqZAU sqNY,Move3 sqTAI sqZAU sqXAI,Move3 sqTAI sqZAU sqXIA,Move3 sqCAI sqCY sqXO,Move3 sqCAI sqCY sqCO,Move3 sqCAI sqCY sqMO,Move3 sqCAI sqCY sqXY,Move3 sqCAI sqCY sqMY,Move3 sqCAI sqCY sqXAI,Move3 sqCAI sqMAI sqMY,Move3 sqCAI sqMAI sqPY,Move3 sqCAI sqMAI sqCAU,Move3 sqCAI sqXAU sqZAI,Move3 sqCAI sqXAU sqXAI,Move3 sqCAI sqXAU sqCAU,Move3 sqCAI sqXAU sqXIA,Move3 sqCAI sqXAU sqCIA,Move3 sqCAI sqMAU sqCAU,Move3 sqCAI sqMAU sqCIA,Move3 sqKAU sqKAI sqKY,Move3 sqKAU sqLAU sqNAU,Move3 sqKAU sqLAU sqLIA,Move3 sqKAU sqKIA sqLIA,Move3 sqLAU sqLAI sqLI,Move3 sqLAU sqLAI sqLU,Move3 sqLAU sqLAI sqLO,Move3 sqLAU sqLAI sqLY,Move3 sqLAU sqLAI sqLIA,Move3 sqLAU sqKAU sqNAU,Move3 sqLAU sqKAU sqTAU,Move3 sqLAU sqZAU sqZAI,Move3 sqLAU sqZAU sqXAI,Move3 sqLAU sqZAU sqXIA,Move3 sqZAU sqZY sqTO,Move3 sqZAU sqZY sqZO,Move3 sqZAU sqZY sqXO,Move3 sqZAU sqZY sqTY,Move3 sqZAU sqZY sqXY,Move3 sqZAU sqZY sqZAI,Move3 sqZAU sqZY sqXAI,Move3 sqZAU sqCY sqXO,Move3 sqZAU sqCY sqCO,Move3 sqZAU sqCY sqMO,Move3 sqZAU sqCY sqXY,Move3 sqZAU sqCY sqMY,Move3 sqZAU sqNAI sqLY,Move3 sqZAU sqNAI sqNY,Move3 sqZAU sqNAI sqTY,Move3 sqZAU sqNAI sqNAU,Move3 sqZAU sqTAI sqLO,Move3 sqZAU sqTAI sqNO,Move3 sqZAU sqTAI sqTO,Move3 sqZAU sqTAI sqZO,Move3 sqZAU sqTAI sqLY,Move3 sqZAU sqTAI sqNY,Move3 sqZAU sqTAI sqTY,Move3 sqZAU sqTAI sqXY,Move3 sqZAU sqTAI sqZAI,Move3 sqZAU sqTAI sqXAI,Move3 sqZAU sqTAI sqNAU,Move3 sqZAU sqTAI sqTAU,Move3 sqZAU sqTAI sqLIA,Move3 sqZAU sqTAI sqXIA,Move3 sqZAU sqCAI sqXY,Move3 sqZAU sqCAI sqMY,Move3 sqZAU sqCAI sqCAU,Move3 sqZAU sqZAU sqZAI,Move3 sqZAU sqZAU sqXAI,Move3 sqZAU sqZAU sqTAU,Move3 sqZAU sqZAU sqXIA,Move3 sqZAU sqXAU sqTY,Move3 sqZAU sqXAU sqXY,Move3 sqZAU sqXAU sqZAI,Move3 sqZAU sqXAU sqXAI,Move3 sqZAU sqXAU sqTAU,Move3 sqZAU sqXAU sqCAU,Move3 sqZAU sqXAU sqXIA,Move3 sqZAU sqXAU sqCIA,Move3 sqZAU sqNIA sqNAU,Move3 sqZAU sqNIA sqLIA,Move3 sqZAU sqTIA sqZAI,Move3 sqZAU sqTIA sqXAI,Move3 sqZAU sqTIA sqNAU,Move3 sqZAU sqTIA sqTAU,Move3 sqZAU sqTIA sqLIA,Move3 sqZAU sqTIA sqXIA,Move3 sqZAU sqZIA sqZAI,Move3 sqZAU sqZIA sqXAI,Move3 sqZAU sqZIA sqNAU,Move3 sqZAU sqZIA sqTAU,Move3 sqZAU sqZIA sqCAU,Move3 sqZAU sqZIA sqXIA,Move3 sqZAU sqZIA sqCIA,Move3 sqXAU sqKI sqLE,Move3 sqXAU sqCAI sqLE,Move3 sqXAU sqCAI sqNI,Move3 sqXAU sqCAI sqTU,Move3 sqXAU sqCAI sqZO,Move3 sqXAU sqCAI sqPO,Move3 sqXAU sqCAI sqXY,Move3 sqXAU sqCAI sqMY,Move3 sqXAU sqZIA sqTAU,Move3 sqMAU sqCAI sqLE,Move3 sqMAU sqCAI sqNI,Move3 sqMAU sqCAI sqTU,Move3 sqMAU sqCAI sqZO,Move3 sqMAU sqCAI sqPO,Move3 sqMAU sqCAI sqXY,Move3 sqMAU sqCAI sqMY,Move3 sqMAU sqCAI sqXAI,Move3 sqMAU sqCAI sqCAU,Move3 sqMAU sqPAI sqXU,Move3 sqMAU sqPAI sqCO,Move3 sqMAU sqPAI sqMY,Move3 sqMAU sqPAI sqCIA,Move3 sqPAU sqPAI sqPY,Move3 sqPAU sqMAU sqCAU,Move3 sqNIA sqPI sqCA,Move3 sqNIA sqPI sqME,Move3 sqTIA sqZAU sqZAI,Move3 sqTIA sqZAU sqXAI,Move3 sqTIA sqZAU sqTAU,Move3 sqTIA sqZAU sqXIA,Move3 sqTIA sqNIA sqNAU,Move3 sqTIA sqNIA sqTAU,Move3 sqTIA sqNIA sqLIA,Move3 sqTIA sqZIA sqTAU,Move3 sqTIA sqZIA sqXIA,Move3 sqZIA sqZAU sqZAI,Move3 sqZIA sqZAU sqXAI,Move3 sqZIA sqZAU sqTAU,Move3 sqZIA sqZAU sqXIA,Move3 sqZIA sqXAU sqZAI,Move3 sqZIA sqXAU sqXAI,Move3 sqZIA sqXAU sqCAU,Move3 sqZIA sqXAU sqXIA,Move3 sqZIA sqXAU sqCIA,Move3 sqZIA sqTIA sqNAU,Move3 sqZIA sqTIA sqTAU,Move3 sqPIA sqMIA sqCIA,Drop (Kok1,Kauk2) sqNA,Drop (Kok1,Kauk2) sqKE,Drop (Kok1,Kauk2) sqNE,Drop (Kok1,Kauk2) sqTE,Drop (Kok1,Kauk2) sqXE,Drop (Kok1,Kauk2) sqCE,Drop (Kok1,Kauk2) sqME,Drop (Kok1,Kauk2) sqNI,Drop (Kok1,Kauk2) sqZI,Drop (Kok1,Kauk2) sqXI,Drop (Kok1,Kauk2) sqKU,Drop (Kok1,Kauk2) sqLU,Drop (Kok1,Kauk2) sqTU,Drop (Kok1,Kauk2) sqZU,Drop (Kok1,Kauk2) sqCU,Drop (Kok1,Kauk2) sqMU,Drop (Kok1,Kauk2) sqPU,Drop (Kok1,Kauk2) sqKO,Drop (Kok1,Kauk2) sqLO,Drop (Kok1,Kauk2) sqNO,Drop (Kok1,Kauk2) sqTO,Drop (Kok1,Kauk2) sqZO,Drop (Kok1,Kauk2) sqXO,Drop (Kok1,Kauk2) sqCO,Drop (Kok1,Kauk2) sqMO,Drop (Kok1,Kauk2) sqPO,Drop (Kok1,Kauk2) sqKY,Drop (Kok1,Kauk2) sqLY,Drop (Kok1,Kauk2) sqNY,Drop (Kok1,Kauk2) sqTY,Drop (Kok1,Kauk2) sqXY,Drop (Kok1,Kauk2) sqMY,Drop (Kok1,Kauk2) sqPY,Drop (Kok1,Kauk2) sqZAI,Drop (Kok1,Kauk2) sqXAI,Drop (Kok1,Kauk2) sqNAU,Drop (Kok1,Kauk2) sqTAU,Drop (Kok1,Kauk2) sqCAU,Drop (Kok1,Kauk2) sqLIA,Drop (Kok1,Kauk2) sqXIA,Drop (Kok1,Kauk2) sqCIA,Drop (Kok1,Nuak1) sqNA,Drop (Kok1,Nuak1) sqKE,Drop (Kok1,Nuak1) sqNE,Drop (Kok1,Nuak1) sqTE,Drop (Kok1,Nuak1) sqXE,Drop (Kok1,Nuak1) sqCE,Drop (Kok1,Nuak1) sqME,Drop (Kok1,Nuak1) sqNI,Drop (Kok1,Nuak1) sqZI,Drop (Kok1,Nuak1) sqXI,Drop (Kok1,Nuak1) sqKU,Drop (Kok1,Nuak1) sqLU,Drop (Kok1,Nuak1) sqTU,Drop (Kok1,Nuak1) sqZU,Drop (Kok1,Nuak1) sqCU,Drop (Kok1,Nuak1) sqMU,Drop (Kok1,Nuak1) sqPU,Drop (Kok1,Nuak1) sqKO,Drop (Kok1,Nuak1) sqLO,Drop (Kok1,Nuak1) sqNO,Drop (Kok1,Nuak1) sqTO,Drop (Kok1,Nuak1) sqZO,Drop (Kok1,Nuak1) sqXO,Drop (Kok1,Nuak1) sqCO,Drop (Kok1,Nuak1) sqMO,Drop (Kok1,Nuak1) sqPO,Drop (Kok1,Nuak1) sqKY,Drop (Kok1,Nuak1) sqLY,Drop (Kok1,Nuak1) sqNY,Drop (Kok1,Nuak1) sqTY,Drop (Kok1,Nuak1) sqXY,Drop (Kok1,Nuak1) sqMY,Drop (Kok1,Nuak1) sqPY,Drop (Kok1,Nuak1) sqZAI,Drop (Kok1,Nuak1) sqXAI,Drop (Kok1,Nuak1) sqNAU,Drop (Kok1,Nuak1) sqTAU,Drop (Kok1,Nuak1) sqCAU,Drop (Kok1,Nuak1) sqLIA,Drop (Kok1,Nuak1) sqXIA,Drop (Kok1,Nuak1) sqCIA,Drop (Kok1,Dau2) sqNA,Drop (Kok1,Dau2) sqKE,Drop (Kok1,Dau2) sqNE,Drop (Kok1,Dau2) sqTE,Drop (Kok1,Dau2) sqXE,Drop (Kok1,Dau2) sqCE,Drop (Kok1,Dau2) sqME,Drop (Kok1,Dau2) sqNI,Drop (Kok1,Dau2) sqZI,Drop (Kok1,Dau2) sqXI,Drop (Kok1,Dau2) sqKU,Drop (Kok1,Dau2) sqLU,Drop (Kok1,Dau2) sqTU,Drop (Kok1,Dau2) sqZU,Drop (Kok1,Dau2) sqCU,Drop (Kok1,Dau2) sqMU,Drop (Kok1,Dau2) sqPU,Drop (Kok1,Dau2) sqKO,Drop (Kok1,Dau2) sqLO,Drop (Kok1,Dau2) sqNO,Drop (Kok1,Dau2) sqTO,Drop (Kok1,Dau2) sqZO,Drop (Kok1,Dau2) sqXO,Drop (Kok1,Dau2) sqCO,Drop (Kok1,Dau2) sqMO,Drop (Kok1,Dau2) sqPO,Drop (Kok1,Dau2) sqKY,Drop (Kok1,Dau2) sqLY,Drop (Kok1,Dau2) sqNY,Drop (Kok1,Dau2) sqTY,Drop (Kok1,Dau2) sqXY,Drop (Kok1,Dau2) sqMY,Drop (Kok1,Dau2) sqPY,Drop (Kok1,Dau2) sqZAI,Drop (Kok1,Dau2) sqXAI,Drop (Kok1,Dau2) sqNAU,Drop (Kok1,Dau2) sqTAU,Drop (Kok1,Dau2) sqCAU,Drop (Kok1,Dau2) sqLIA,Drop (Kok1,Dau2) sqXIA,Drop (Kok1,Dau2) sqCIA]





str = 
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n\
 \_7k_2k - _4k - _4h - _2h_7h\n\
 \_1h_1k - _1k - _1k_1h_1k_1h\n\
 \ - _1h - ^$h -  -  -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  - ^4h -  -  -  -  - \n\
 \^1h^1k^1h^1k^!h^1k^1h^1k^1h\n\
 \^7h -  - ^2h - ^4k - ^2k^7k\n\
 \^6k^5k^3k^8k_!k^8h^3h^5h^6h\n\
 \~~~\n\
 \_#h\n"

fed001'' :: Operation ()
fed001'' = do
 vPlays3'      sqTAU 虎 sqNAI sqTY  >+> vPlays3'      sqXE 虎 sqCI sqXU
 vPlays3'      sqLIA 馬 sqTAI sqXO  >+> mun1(vPlays3' sqXI 兵 sqXU sqXO)
 vPlays2'      sqZAI 船 sqZY  >+> vPlays2'      sqME 弓 sqZE
 mun1(vPlays3' sqXIA 将 sqXAU sqZAI)>+> vPlTam2    sqZO    sqZAI
 vPlays2'      sqCAI 兵 sqCY  >+> mun1(vPlays3' sqXI 兵 sqXU sqXO) 
 vPlays3'      sqXIA 将 sqXAU sqCAI >+> mun1(vPlays3' sqXI 兵 sqXU sqXO) 
 vPlays2'      sqCAI 将 sqXY  >+> vPlays2'      sqXU 虎 sqTY
 vPlays2'      sqTAI 兵 sqTY  >+> vPlays3'      sqTE 虎 sqZI sqTU
 vPlays3'      sqXY  将 sqXAI sqCAU >+> vPlays2'      sqTU 虎 sqMAU
 vPlays2'      sqCIA 車 sqMAU >+> vPlTam3    sqZAI   sqXAI sqXY
 vPlays2'      sqCAU 将 sqCAI >+> vPlays2'      sqZI 船 sqZY
 vPlTam2       sqXY     sqZAU >+> vPlays3'      sqKE 巫 sqLE sqNE
 drops        (黒, 虎) sqTAI >+> vPlays2'      sqNI 兵 sqNU
 vPlays2'      sqXAI 兵 sqXY  >+> vPlays2'      sqNE 巫 sqNI
 vPlays2'      sqTY  兵 sqZY  >+> mun1(vPlays2' sqZE 弓 sqZY)
 mun1(vPlays2' sqNIA 車 sqZAI)>+> vPlays2'      sqNA 車 sqXU
 vPlays2'      sqXO  馬 sqMI  >+> vPlays2'      sqCI 兵 sqMI
 declare Downward Saup1
 vPlays2'      sqXY  兵 sqXU  >+> vPlays2'      sqXI 兵 sqXU
 drops'            車 sqCI  >+> vPlays2'      sqNI 巫 sqCI
 declare Downward Mok1Mok1
 taxot1


fed002'' :: Operation ()
fed002'' = do
 vPlays3'      sqKE  Tuk2  sqLE  sqNE >-> vPlays2'      sqTAI Kauk2       sqTY
 vPlays2'      sqNI  Kauk2       sqNU >-> vPlTam3       sqZO        sqTY  sqZAU
 vPlays2'      sqNE  Tuk2        sqNI >-> vPlays2'      sqNAI Kauk2       sqNY
 vPlays2'      sqNI  Tuk2        sqTU >-> vPlays3'      sqKAU Tuk2  sqLAU sqNAU
 vPlays3'      sqTA  Uai1  sqTE  sqNI >-> vPlays2'      sqXAI Kauk2       sqXY
 vPlays3'      sqTE  Dau2  sqNI  sqLU >-> vPlays2'      sqNAU Tuk2        sqNAI
 vPlays3'      sqXE  Dau2  sqCI  sqXU >-> vPlays2'      sqCAI Kauk2       sqCY
 vPlays3'      sqPE  Tuk2  sqME  sqCE >-> vPlays3'      sqXIA Uai1  sqXAU sqXAI
 vPlays2'      sqCI  Kauk2       sqCU >-> vPlays3'      sqTIA Uai1  sqTAU sqTAI
 vPlays3'      sqXA  Uai1  sqCE  sqCI >-> vPlays3'      sqTAU Dau2  sqNAI sqLY
 vPlays3'      sqMA  Maun1 sqXI  sqTO >-> vPlays2'      sqTY  Kauk2       sqTO
 vPlTam3       sqZAU       sqTAI sqZO >-> vPlTam3       sqZO        sqXU  sqZY
 vPlays2'      sqLE  Gua2        sqZE >-> vPlays3'      sqZAI Nuak1 sqXAI sqCAI
 vPlays3'      sqLA  Maun1 sqTI  sqLO >-> mun1(vPlays2' sqXAI Uai1        sqZAI)
 vPlays3'      sqKA  Kua2  sqKI  sqKU >-> vPlays3'      sqLAI Kauk2 sqLY  sqLO
 vPlays3'      sqLI  Kauk2 sqLU  sqLO >-> vPlays3'      sqLY  Dau2  sqNAI sqTY
 mun1(vPlays3' sqZE  Gua2  sqZI  sqZU)>-> vPlays3'      sqZIA Io    sqXAU sqCAU
 vPlTam2       sqZY              sqCO >-> vPlTam3       sqCO        sqXY  sqZAU
 vPlays2'      sqZI  Nuak1       sqZO >-> vPlays2'      sqTY  Dau2        sqZO
 mun1(vPlays2' sqZE  Gua2        sqZO)>-> vPlays3'      sqZO  Dau2  sqTU  sqNI
 vPlays2'      sqTU  Tuk2        sqNI >-> vPlays3'      sqTAI Uai1  sqNY  sqLO
 vPlays3'      sqLU  Dau2  sqNI  sqTE >-> vPlays2'      sqNAI Tuk2        sqCI
 vPlays2'      sqCE  Tuk2        sqCI >-> vPlays2'      sqXY  Kauk2       sqXU
 declare Upward Saup1 
 taxot1 



fed003'' :: Operation ()
fed003'' = do
 vPlays2'   sqXIA Uai1  sqZAU >+> vPlays2' sqTI Kauk2 sqTU
 vPlTam2    sqZO        sqCY  >+> vPlays2' sqXI Kauk2 sqXU
 vPlays2'   sqMAI Kauk2 sqMY  >+> vPlays2' sqNI Kauk2 sqNU
 vPlTam2    sqCY        sqCAU >+> vPlays3' sqKE Tuk2  sqLE sqNE
 vPlays2'   sqCAI Kauk2 sqCY  >+> vPlays2' sqNE Tuk2  sqNI
 vPlays2'   sqZAI Nuak1 sqZY  >+> vPlays2' sqZI Nuak1 sqZY
 vPlays3'   sqZAU Uai1  sqTAI sqZY  >+> vPlays3' sqLE Gua2 sqTE sqZE
 vPlays3'   sqZY  Uai1  sqXAI sqCAI >+> vPlays2' sqZE Gua2  sqZIA
 declare Downward Dat2AIo 
 taxot1 

fed003''' :: Operation ()
fed003''' = do
 vPlays2 sqXIA       sqZAU >+> vPlays2 sqTI      sqTU
 vPlays2 sqZO        sqCY  >+> vPlays2 sqXI      sqXU
 vPlays2 sqMAI       sqMY  >+> vPlays2 sqNI      sqNU
 vPlays2 sqCY        sqCAU >+> vPlays3 sqKE sqLE sqNE
 vPlays2 sqCAI       sqCY  >+> vPlays2 sqNE      sqNI
 vPlays2 sqZAI       sqZY  >+> vPlays2 sqZI      sqZY
 vPlays3 sqZAU sqTAI sqZY  >+> vPlays3 sqLE sqTE sqZE
 vPlays3 sqZY  sqXAI sqCAI >+> vPlays2 sqZE      sqZIA
 declare Downward Dat2AIo 
 taxot1 



fed004'' :: Operation ()
fed004'' = do
 vPlays2' sqTAI Kauk2 sqTY  >+> vPlays2' sqXI Kauk2 sqXU
 vPlays2' sqXAI Kauk2 sqXY  >+> vPlays2' sqZI Nuak1 sqZU
 vPlays2' sqZAI Nuak1 sqZY  >+> vPlTam3  sqZO  sqZU   sqXI
 vPlays2' sqXAU Dau2  sqZAI >+> vPlays3' sqXA Uai1 sqXE sqZI
 vPlays2' sqMAU Gua2  sqZAU >+> vPlays3' sqLE Gua2 sqTE sqZE
 vPlays2' sqXIA Uai1  sqXAU >+> vPlays3' sqZA Io sqTE sqNE
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


fed005'' :: Operation ()
fed005'' = do
 vPlays3' sqTE 虎 sqNI sqTU  >-> vPlays2' sqTAI 兵 sqTY
 vPlays2' sqZI 船 sqZU  >-> vPlays3' sqTAU 虎 sqZAI sqXY
 vPlTam3  sqZO sqXI sqCE  >-> vPlays2' sqXIA 将 sqZAU
 vPlays2' sqLE 弓 sqZE  >-> vPlays2' sqTY  兵 sqTU
 vPlays2' sqTI 兵 sqTU  >-> vPlays2' sqZIA 王 sqXIA
 vPlays3' sqME 弓 sqMI sqMU  >-> vPlays2' sqXY  虎 sqMU
 vPlays2' sqPE 巫 sqME  >-> vPlays2' sqLIA 馬 sqTAI
 vPlays2' sqMI 兵 sqMU  >-> vPlays3' sqLAU 弓 sqLAI sqLY
 vPlays2' sqZE 弓 sqTE  >-> mun1(vPlays2' sqLY  弓 sqZY)
 vPlays2' sqTU 兵 sqTY  >-> vPlays2' sqZAI 船 sqZU
 vPlays2' sqTY 兵 sqTAI Downward
 declare Downward Saup1 
 taxot1

fed005''' :: Operation ()
fed005''' = do
 vPlays3 sqTE sqNI sqTU  >-> vPlays2 sqTAI       sqTY
 vPlays2 sqZI      sqZU  >-> vPlays3 sqTAU sqZAI sqXY
 vPlays3 sqZO sqXI sqCE  >-> vPlays2 sqXIA       sqZAU
 vPlays2 sqLE      sqZE  >-> vPlays2 sqTY        sqTU
 vPlays2 sqTI      sqTU  >-> vPlays2 sqZIA       sqXIA
 vPlays3 sqME sqMI sqMU  >-> vPlays2 sqXY        sqMU
 vPlays2 sqPE      sqME  >-> vPlays2 sqLIA       sqTAI
 vPlays2 sqMI      sqMU  >-> vPlays3 sqLAU       sqLAI sqLY
 vPlays2 sqZE      sqTE  >-> mun1(vPlays2 sqLY  sqZY)
 vPlays2 sqTU      sqTY  >-> vPlays2 sqZAI sqZU
 vPlays2 sqTY      sqTAI Downward
 declare Downward Saup1 
 taxot1 


