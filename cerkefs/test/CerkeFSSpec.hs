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
    it "random (new automatic) 087" $ 
      "_6h_5h_3h_8h^!h_8k_3k_5k_6k\n\
      \_7k - _2k_4k - _4h - _2h_7h\n\
      \_1h_1k - _1k_!k_1k_1h_1k_1h\n\
      \ - _1h -  -  -  -  -  -  - \n\
      \ -  -  -  -  -  -  -  -  - \n\
      \ - ^1h -  -  - ^$h -  -  - \n\
      \^1h^1k - ^1k - ^1k^1h^1k^1h\n\
      \^7h^2h - ^4h - ^4k - ^2k^7k\n\
      \^6k^5k^3k^8k^#h^8h^3h^5h^6h\n\
      \~~~\n\
      \^#k\n" `shouldBeGeneratedBy` do
       vPlays3 sqZO sqZO sqXY Upward
       vPlays2 sqLE sqNE Downward
       vPlays3 sqNAI sqLAI sqLY Upward
       vPlays3 sqNI sqLI sqLU Downward
       vPlays3 sqZAI sqZI sqZA Upward
       declare Upward Dat2AIo
    it "random (new automatic) 101" $
      "_6h_5h - _8h -  - ^3h - _6k\n\
      \ -  -  - _4k_1h_3h_4k_8k^$h\n\
      \_1h - _1h_1k_!k -  - _1h_1h\n\
      \_7k - _1k - _#k - _1k -  - \n\
      \^3k -  - _5k - ^8h - _1k_7h\n\
      \^2h^4h^8k^2h^!h - ^1h -  - \n\
      \^1h^1k - ^1k - ^1k - ^1k^1h\n\
      \^7h -  -  -  -  - ^2k - ^7k\n\
      \^6k^5k -  -  -  - _4h^5h^6h\n\
      \~~~\n\
      \_#h^2k^3k\n" `shouldBeGeneratedBy` do
        vPlays2 sqNAI sqNY Upward
        vPlays2 sqMI sqMU Downward
        vPlays3 sqZO sqNY sqLO Upward
        vPlays2 sqLO sqTO Downward
        vPlays2 sqTIA sqNAU Upward
        vPlays2 sqCI sqMI Downward
        vPlays3 sqTO sqLI sqNE Upward
        vPlays2 sqXE sqCI Downward
        vPlays2 sqCAI sqCY Upward
        vPlays3 sqNE sqLI sqLU Downward
        vPlays3 sqXIA sqXAU sqCAU Upward
        vPlays3 sqLI sqNI sqNU Downward
        vPlays3 sqCAU sqMAI sqCAI Upward
        vPlays3 sqLU sqNY sqTO Downward
        vPlays3 sqTO sqZI sqZE Upward
        vPlays2 sqZA sqXE Downward
        vPlays3 sqZE sqXE sqCE Upward
        vPlays3 sqME sqMI sqCU Downward
        vPlays3 sqXAU sqZAI sqTY Upward
        vPlays3 sqCE sqPI sqPU Downward
        vPlays3 sqNIA sqTAU sqKO Upward
        vPlays3 sqCU sqNU sqNY Downward
        vPlays3 sqTY sqZAI sqXY Upward
        vPlays3 sqPU sqMI sqME Downward
        vPlays3 sqLAU sqNAU sqNAI Upward
        vPlays3 sqCA sqMA sqCE Downward
        vPlays3 sqXY sqCAI sqZO Upward
        vPlays3 sqLE sqLA sqLY Downward
        vPlays3 sqNAI sqNY sqZY Upward
        vPlays3 sqTE sqNI sqZO Downward
        vPlays3 sqCIA sqZAI sqCO Upward
        drops (Huok2,Kauk2) sqZA Downward
        vPlays3 sqZY sqCY sqNY Upward
        vPlays3 sqXE sqCI sqCU Downward
        vPlays2 sqZIA sqXAU Upward
        vPlays3 sqPE sqPA sqPU Downward
        vPlays3 sqZAI sqZO sqZIA Upward
        vPlays3 sqCE sqMA sqCA Downward
        vPlays2 sqNAU sqNAI Upward
        vPlays2 sqZA sqZE Downward
        vPlays3 sqCO sqZI sqCA Upward
        vPlays3 sqKE sqKI sqKU Downward
        vPlays3 sqMAU sqXAU sqXIA Upward
        vPlays3 sqNA sqZI sqXE Downward
        drops (Huok2,Gua2) sqKY Upward
        vPlays2 sqCU sqXU Downward
        vPlays3 sqTAU sqNAI sqLY Upward
        drops (Kok1,Dau2) sqCE Downward
        vPlays2 sqZIA sqZY Upward
        vPlays2 sqMU sqMO Downward
        vPlays2 sqXAU sqCIA Upward
        vPlays2 sqXU sqZU Downward
        vPlays3 sqNY sqNU sqLU Upward
        vPlays3 sqME sqPA sqPE Downward
        vPlays2 sqCAI sqXY Upward
        vPlays3 sqPU sqPI sqPO Downward
        vPlays3 sqLU sqLY sqTY Upward
        vPlays2 sqZO sqXU Downward
        vPlays3 sqXIA sqCIA sqCAI Upward
        vPlays3 sqXA sqCE sqME Downward
        vPlays3 sqNAI sqLY sqNY Upward
        vPlays3 sqMA sqXI sqTO Downward
        vPlays2 sqCAI sqMAU Upward
        vPlays3 sqXI sqXU sqCU Downward
        vPlays3 sqXY sqZY sqXO Upward
        vPlays3 sqXU sqZI sqTE Downward
        vPlays3 sqMAU sqPAU sqCAU Upward
        vPlays3 sqCI sqTY sqCIA Downward
        declare Downward Dat2AIo
    it "random (new automatic) 036" $
      "_6h - _3h_8h - _5h_3k - _6k\n\
      \ -  - _#k_4k - _4h - _2h_7h\n\
      \_1h^7h_1h_1k - _1k -  - _1h\n\
      \ - _1k - _!h -  - _1k_1h^5k\n\
      \ -  -  -  -  -  -  - _8k - \n\
      \ -  - _7k - ^1k - ^3k -  - \n\
      \^1h^1k^1h - ^$h^1k^1h^1k^1h\n\
      \ - ^4h^2h^4k -  - ^8h^2k^7k\n\
      \^6k^5k - _!k_2k - ^3h^5h^6h\n\
      \~~~\n\
      \_#h_8k\n" `shouldBeGeneratedBy` do
        vPlays2 sqTAI sqTY Upward
        vPlays3 sqZO sqZO sqTU Downward
        vPlays3 sqLAU sqTAU sqZAU Upward
        vPlays3 sqTU sqTI sqTO Downward
        vPlays3 sqTAU sqNIA sqLAU Upward
        vPlays3 sqTO sqTY sqZU Downward
        vPlays3 sqZU sqZI sqCO Upward
        vPlays3 sqCO sqMI sqCE Downward
        vPlays3 sqCE sqXI sqZO Upward
        vPlays2 sqLI sqLU Downward
        vPlays3 sqZAU sqZAI sqTAI Upward
        vPlays3 sqLE sqLU sqPU Downward
        vPlays3 sqZO sqNI sqNU Upward
        vPlays2 sqNU sqTU Downward
        vPlays3 sqXIA sqCIA sqCAU Upward
        vPlays3 sqTU sqZI sqXU Downward
        vPlays2 sqTAI sqTAU Upward
        vPlays3 sqXA sqXE sqCE Downward
        vPlays2 sqTY sqZY Upward
        vPlays3 sqMA sqXI sqXA Downward
        vPlays2 sqXU sqMO Upward
        vPlays3 sqMO sqXAI sqZAU Downward
        vPlays2 sqZAU sqTY Upward
        vPlays2 sqTY sqTU Downward
        vPlays3 sqTU sqLU sqKO Upward
        vPlays3 sqCE sqCI sqCU Downward
        vPlays2 sqKO sqKY Upward
        vPlays3 sqCI sqMI sqMU Downward
        vPlays3 sqKY sqKAI sqLO Upward
        vPlays3 sqLO sqLAI sqLY Downward
        vPlays3 sqKAU sqKAI sqNO Upward
        vPlays3 sqTE sqNI sqTU Downward
        vPlays3 sqNO sqNI sqLE Upward
        vPlays2 sqCU sqMO Downward
        vPlays2 sqLE sqKE Upward
        vPlays3 sqLY sqLAI sqTO Downward
        drops (Kok1,Tuk2) sqZE Upward
        vPlays3 sqTO sqZY sqXY Downward
        vPlays3 sqXY sqMO sqPO Upward
        vPlays3 sqMI sqMU sqCU Downward
        vPlays2 sqKIA sqKAU Upward
        vPlays3 sqPU sqPO sqPY Downward
        vPlays3 sqPO sqMO sqXY Upward
        vPlays3 sqPY sqXY sqZO Downward
        vPlays3 sqZAI sqXAI sqTAI Upward
        vPlays3 sqTU sqNI sqTE Downward
        vPlays3 sqZE sqXE sqXA Upward
        vPlays3 sqXY sqZY sqZAU Downward
        vPlays3 sqNIA sqTAU sqCO Upward
        vPlays2 sqMO sqPU Downward
        vPlays3 sqTAI sqTAU sqTY Upward
        vPlays2 sqPU sqMO Downward
        vPlays3 sqZAU sqZIA sqTAI Upward
        vPlays3 sqLA sqTI sqXA Downward
        drops (Kok1,Maun1) sqPU Upward
        vPlays3 sqZA sqTA sqNE Downward
        vPlays3 sqTAU sqTAI sqNAU Upward
        vPlays3 sqZI sqZO sqTO Downward
        vPlays2 sqKAU sqKIA Upward
        vPlays3 sqZO sqZY sqTY Downward
        vPlays3 sqTAI sqTO sqNO Upward
        vPlays3 sqNO sqLU sqNU Downward
        vPlays3 sqNU sqTO sqXY Upward
        drops (Huok2,Nuak1) sqTU Downward
        vPlays3 sqXY sqXAU sqXIA Upward
        vPlays3 sqXIA sqXAI sqCY Downward
        vPlays3 sqCY sqCY sqMY Upward
        drops (Kok1,Tuk2) sqNY Downward
        vPlays3 sqKE sqKI sqLI Upward
        vPlays3 sqTO sqTY sqTIA Downward
        vPlays3 sqCO sqMO sqCY Upward
        vPlays3 sqMY sqCY sqZAI Downward
        vPlays3 sqXAU sqZIA sqTAU Upward
        vPlays3 sqTY sqTAU sqZIA Downward
        declare Downward Dat2AIo
    it "random (new automatic) 017" $
      "^2h -  -  - ^6h - ^5k_6k - \n\
      \ - _4h - _4k -  - _8h_2h_1h\n\
      \ - _1h^3h_2k - ^3k - _1k - \n\
      \_1k - _1h - ^!h_8k^1h -  - \n\
      \^3k_1k_1k_5k -  -  -  -  - \n\
      \^2k - _1h^5h_7k^1k - ^4k_7h\n\
      \_8h^1k - ^$h - ^1k - ^1k_3h\n\
      \^7h^4h^8k -  -  -  -  - ^7k\n\
      \^6k^#h -  -  - ^1h^!k^5h^1h\n\
      \~~~\n\
      \^#k^6h_1h\n" `shouldBeGeneratedBy` do
        vPlays3 sqZO sqZO sqTU Upward
        vPlays3 sqLE sqTE sqZE Downward
        vPlays3 sqTU sqNI sqLO Upward
        vPlays2 sqKE sqLE Downward
        vPlays2 sqTIA sqNAU Upward
        vPlays3 sqPE sqPI sqPU Downward
        vPlays3 sqLO sqKAI sqKY Upward
        vPlays3 sqKY sqKY sqLY Downward
        vPlays3 sqLY sqLAI sqNO Upward
        vPlays3 sqNO sqTI sqNE Downward
        vPlays3 sqLAU sqLAI sqLU Upward
        vPlays3 sqLI sqLE sqKE Downward
        vPlays3 sqMAU sqXAU sqZAU Upward
        vPlays3 sqLE sqNE sqNAI Downward
        vPlays3 sqNIA sqTAU sqNAI Upward
        vPlays3 sqNE sqNE sqLI Downward
        vPlays3 sqNAI sqCI sqMU Upward
        vPlays2 sqCI sqCU Downward
        vPlays3 sqPIA sqMIA sqMAU Upward
        vPlays3 sqLI sqLU sqNO Downward
        vPlays2 sqMAU sqCAU Upward
        vPlays3 sqNO sqLU sqKY Downward
        vPlays3 sqLU sqLAI sqNAI Upward
        vPlays3 sqKY sqLAI sqLY Downward
        vPlays3 sqNAI sqNI sqTE Upward
        vPlays3 sqXE sqZI sqTU Downward
        vPlays3 sqLY sqLAI sqNAI Upward
        drops (Huok2,Kauk2) sqLE Downward
        vPlays3 sqZAU sqZAI sqZU Upward
        vPlays3 sqZE sqTE sqXE Downward
        vPlays3 sqXAU sqCIA sqMAU Upward
        vPlays3 sqMA sqXI sqTO Downward
        vPlays2 sqZU sqTU Upward
        vPlays3 sqME sqMI sqMA Downward
        vPlays3 sqNAI sqTAI sqLO Upward
        vPlays3 sqLO sqLO sqLY Downward
        vPlays3 sqLIA sqTAI sqXO Upward
        vPlays3 sqTA sqNA sqNE Downward
        drops (Huok2,Dau2) sqLAU Upward
        vPlays2 sqNI sqLI Downward
        vPlays3 sqMAU sqCAI sqMY Upward
        vPlays3 sqNA sqTE sqLU Downward
        vPlays2 sqXAI sqXY Upward
        vPlays3 sqLU sqZAI sqCIA Downward
        vPlays3 sqCAU sqCAI sqCU Upward
        vPlays3 sqLY sqTO sqZO Downward
        drops (Huok2,Kauk2) sqLY Upward
        vPlays3 sqZO sqTU sqTY Downward
        drops (Kok1,Dau2) sqTA Upward
        vPlays3 sqZA sqTE sqNA Downward
        vPlays3 sqTY sqTO sqZU Upward
        vPlays3 sqNA sqTA sqZA Downward
        vPlays3 sqZU sqCU sqMO Upward
        vPlays3 sqPU sqMU sqZAI Downward
        vPlays3 sqMO sqMO sqCY Upward
        vPlays3 sqCY sqXY sqZAU Downward
        vPlays3 sqTAU sqZAI sqKI Upward
        vPlays2 sqZAI sqCO Downward
        vPlays3 sqCU sqCA sqXA Upward
        vPlays3 sqNE sqLI sqLU Downward
        vPlays3 sqCAI sqCO sqCU Upward
        drops (Huok2,Kaun1) sqNO Downward
        drops (Huok2,Kauk2) sqPIA Upward
        vPlays2 sqTI sqTU Downward
        vPlays3 sqNAU sqLAU sqNIA Upward
        vPlays3 sqNO sqTU sqLY Downward
        drops (Kok1,Tuk2) sqXU Upward
        drops (Huok2,Nuak1) sqTIA Downward
        vPlays3 sqZAU sqXY sqCY Upward
        vPlays3 sqCY sqMAI sqPY Downward
        drops (Kok1,Uai1) sqZE Upward
        vPlays2 sqCO sqXO Downward
        vPlays3 sqMU sqXY sqCY Upward
        drops (Kok1,Gua2) sqLO Downward
        vPlays2 sqXA sqXE Upward
        drops (Kok1,Maun1) sqZU Downward
        vPlays2 sqPY sqMU Upward
        vPlays2 sqLY sqNO Downward
        vPlays3 sqMU sqCU sqCO Upward
        drops (Huok2,Kauk2) sqNY Downward
        vPlays3 sqCO sqXY sqMU Upward
        vPlays3 sqLU sqLO sqKU Downward
        vPlays3 sqMU sqCU sqCI Upward
        vPlays3 sqZU sqNY sqZAU Downward
        vPlays3 sqXIA sqZIA sqZAU Upward
        vPlays3 sqCI sqPI sqPE Downward
        vPlays3 sqCY sqZAU sqMO Upward
        vPlays3 sqLO sqLAI sqNAI Downward
        vPlays3 sqXU sqCU sqCI Upward
        vPlays2 sqXO sqCO Downward
        vPlays3 sqZE sqTE sqZI Upward
        vPlays3 sqNAI sqLAU sqNAU Downward
        vPlays3 sqPE sqMI sqME Upward
        vPlays2 sqNO sqLY Downward
        drops (Kok1,Gua2) sqNO Upward
        vPlays3 sqNAU sqZAU sqZI Downward
        drops (Kok1,Maun1) sqNI Upward
        vPlays3 sqZI sqXI sqXU Downward
        vPlays3 sqCI sqPA sqPE Upward
        vPlays3 sqZA sqTE sqNA Downward
        vPlays2 sqKI sqLE Upward
        vPlays3 sqKU sqLI sqNE Downward
        vPlays3 sqPE sqMA sqZA Upward
        vPlays3 sqXU sqTU sqTI Downward
        vPlays3 sqME sqCA sqCI Upward
        vPlays3 sqCI sqXE sqCE Downward
        vPlays2 sqTA sqZE Upward
        vPlays2 sqLY sqNAI Downward
        vPlays3 sqCE sqMI sqPE Upward
        vPlays3 sqNE sqLA sqLE Downward
        vPlays3 sqTE sqLE sqLA Upward
        vPlays3 sqLE sqNA sqTE Downward
        drops (Kok1,Nuak1) sqCAI Upward
        vPlays3 sqPE sqMI sqME Downward
        vPlays3 sqMO sqZAU sqPU Upward
        drops (Kok1,Uai1) sqXA Downward
        drops (Huok2,Maun1) sqXAU Upward
        vPlays3 sqTE sqTI sqZE Downward
        vPlays2 sqZIA sqTIA Upward
        vPlays2 sqCIA sqPAI Downward
        vPlays3 sqME sqXE sqCI Upward
        vPlays2 sqCI sqME Downward
        vPlays2 sqXAU sqTY Upward
        vPlays3 sqNAI sqTAI sqZAU Downward
        drops (Huok2,Nuak1) sqZU Upward
        vPlays2 sqMA sqPE Downward
        vPlays3 sqXE sqZE sqZI Upward
        drops (Huok2,Dau2) sqLE Downward
        vPlays3 sqNIA sqTIA sqTAU Upward
        vPlays3 sqME sqPI sqPO Downward
        vPlays3 sqTIA sqZAU sqXAU Upward
        vPlays3 sqPO sqMY sqXO Downward
        vPlays3 sqXO sqMAI sqMAU Upward
        drops (Huok2,Kauk2) sqNU Downward
        vPlays3 sqMAU sqMAI sqPY Upward
        vPlays3 sqXA sqZE sqXE Downward
        vPlays2 sqCAI sqCIA Upward
        vPlays3 sqXE sqXI sqZI Downward
        vPlays2 sqLA sqKA Upward
        vPlays3 sqZE sqXI sqCE Downward
        vPlays3 sqPY sqPAI sqMO Upward
        vPlays2 sqPE sqME Downward
        vPlays3 sqNI sqZI sqCA Upward
        drops (Huok2,Uai1) sqLY Downward
        vPlays3 sqMO sqPU sqMU Upward
        vPlays2 sqZAU sqTAI Downward
        drops (Huok2,Kua2) sqTA Upward
        vPlays2 sqPI sqPE Downward
        drops (Huok2,Kauk2) sqXIA Upward
        vPlays3 sqCO sqCU sqXU Downward
        drops (Kok1,Kaun1) sqKO Upward
        vPlays2 sqKE sqKI Downward
        vPlays3 sqPU sqMU sqMO Upward
        vPlays3 sqMU sqCU sqZY Downward
        vPlays2 sqNO sqLO Upward
        drops (Huok2,Kua2) sqLIA Downward
        vPlays3 sqLO sqKO sqKY Upward
        vPlays2 sqPA sqMA Downward
        vPlays2 sqZA sqZE Upward
        vPlays3 sqZY sqXAU sqZAI Downward
        vPlays2 sqTAU sqNAU Upward
        drops (Kok1,Kauk2) sqLO Downward
        vPlays3 sqXAU sqZAI sqTAI Upward
        vPlays2 sqKI sqKU Downward
        vPlays3 sqZAI sqXIA sqXAU Upward
        vPlays2 sqXAU sqTIA Downward
        vPlays3 sqTIA sqXIA sqCAU Upward
        vPlays3 sqCAU sqXY sqZAI Downward
        vPlays3 sqZAI sqTO sqZO Upward
        vPlays3 sqLY sqLAI sqKAI Downward
        vPlays2 sqZO sqCO Upward
        vPlays3 sqCO sqMO sqXO Downward
        vPlays3 sqMO sqCU sqXI Upward
        vPlays3 sqTU sqNU sqNO Downward
        vPlays2 sqTA sqZA Upward
        drops (Kok1,Dau2) sqTE Downward
        drops (Huok2,Kaun1) sqZY Upward
        vPlays3 sqNA sqTE sqZE Downward
        drops (Kok1,Kauk2) sqXAI Upward
        vPlays3 sqXU sqPAI sqPY Downward
        vPlays3 sqTAI sqNAU sqLIA Upward
        vPlays3 sqZE sqTE sqNI Downward
        vPlays3 sqZY sqTY sqNAI Upward
        drops (Kok1,Tuk2) sqZY Downward
        vPlays3 sqXO sqXY sqTAI Upward
        vPlays2 sqZI sqXU Downward
        vPlays3 sqNAI sqKO sqNI Upward
        declare Upward Dat2AIo
    it "random (new automatic) 109" $
       " - ^1k_4k_8h - _2h_3k_7h_6k\n\
       \_7k_6h^5h - ^!h_5k_8h_!k - \n\
       \_2k_1k - _1k -  - ^1h_1k_1h\n\
       \ -  - ^4k^4h -  -  -  -  - \n\
       \^3h - ^1h^8k -  -  -  - ^1h\n\
       \ -  - ^1k - ^1k -  -  - _2h\n\
       \^1h -  - _3h^$h - ^1h - ^6h\n\
       \ - ^7h^#h^8k -  -  - ^2k - \n\
       \^6k -  -  - ^1k_1h^7k_#k^1h\n\
       \~~~\n\
       \_4h^5k_5h_1k_3k\n" `shouldBeGeneratedBy` do
        vPlays3 sqZO sqTI sqZE Downward
        vPlays3 sqLIA sqTAI sqLO Upward
        vPlays3 sqME sqPE sqCE Downward
        vPlays3 sqZE sqZI sqXU Upward
        vPlays2 sqCI sqCO Downward
        vPlays3 sqXU sqXI sqTO Upward
        vPlays2 sqCO sqCY Downward
        vPlays3 sqZAI sqZI sqZE Upward
        vPlays3 sqTO sqNI sqLU Downward
        vPlays2 sqTIA sqZAU Upward
        vPlays2 sqLU sqNU Downward
        vPlays3 sqLO sqTI sqXI Upward
        vPlays2 sqNU sqKU Downward
        drops (Kok1,Kauk2) sqNY Upward
        vPlays2 sqZI sqZAU Downward
        vPlays3 sqTAU sqNAI sqCI Upward
        vPlays3 sqNA sqTE sqXU Downward
        vPlays3 sqXAU sqCAI sqTU Upward
        drops (Kok1,Uai1) sqXAU Downward
        vPlays2 sqXI sqMA Upward
        vPlays3 sqNI sqTI sqTU Downward
        vPlays3 sqZIA sqXAU sqZAU Upward
        vPlays3 sqXU sqNAI sqLY Downward
        vPlays3 sqLAI sqLY sqLO Upward
        vPlays2 sqCA sqME Downward
        vPlays3 sqCI sqNAI sqTAU Upward
        vPlays3 sqXA sqXE sqZI Downward
        vPlays2 sqLO sqLI Upward
        drops (Kok1,Dau2) sqLU Downward
        drops (Kok1,Nuak1) sqCU Upward
        vPlays3 sqCE sqCU sqXU Downward
        drops (Kok1,Kauk2) sqCAU Upward
        vPlays3 sqXU sqXE sqXAI Downward
        drops (Kok1,Maun1) sqCA Upward
        vPlays3 sqXAU sqZAU sqXIA Downward
        vPlays3 sqLI sqLE sqLA Upward
        vPlays3 sqKU sqNY sqTO Downward
        vPlays2 sqNAI sqLAI Upward
        drops (Huok2,Uai1) sqZY Downward
        vPlays3 sqTO sqZY sqNU Upward
        vPlays2 sqLU sqTY Downward
        drops (Huok2,Maun1) sqNE Upward
        vPlays3 sqNU sqTU sqZO Downward
        vPlays2 sqCA sqZI Upward
        vPlays3 sqZA sqZE sqXI Downward
        vPlays3 sqZO sqTU sqXO Upward
        vPlays2 sqTU sqTO Downward
        vPlays3 sqLAU sqTAU sqTIA Upward
        vPlays3 sqXO sqCY sqZO Downward
        vPlays3 sqCIA sqTY sqLU Upward
        vPlays3 sqZO sqZY sqTU Downward
        drops (Kok1,Uai1) sqNU Upward
        vPlays3 sqTU sqTU sqZO Downward
        vPlays2 sqZO sqXU Upward
        drops (Kok1,Kauk2) sqMY Downward
        vPlays3 sqXU sqTO sqZU Upward
        vPlays3 sqLY sqTAU sqKO Downward
        vPlays3 sqZAU sqXIA sqXAU Upward
        vPlays3 sqXI sqZU sqZI Downward
        vPlays3 sqZU sqCY sqMO Upward
        vPlays2 sqTY sqCI Downward
        vPlays3 sqMO sqPI sqPU Upward
        vPlays2 sqZI sqXU Downward
        vPlays3 sqNU sqTO sqZU Upward
        vPlays3 sqPU sqCI sqCE Downward
        vPlays3 sqCE sqMI sqMO Upward
        vPlays3 sqXU sqCU sqXI Downward
        vPlays3 sqLU sqTE sqNA Upward
        vPlays3 sqMO sqMY sqPO Downward
        vPlays3 sqCU sqCI sqCY Upward
        vPlays3 sqPO sqPO sqMU Downward
        vPlays3 sqMU sqMY sqPO Upward
        vPlays3 sqPO sqCY sqXY Downward
        vPlays2 sqZU sqXU Upward
        vPlays3 sqXE sqCI sqXU Downward
        vPlays3 sqXY sqXAI sqZO Upward
        vPlays3 sqXAI sqXU sqNU Downward
        vPlays3 sqZO sqZO sqXY Upward
        vPlays3 sqXY sqTAI sqNAU Downward
        vPlays3 sqNA sqTE sqKO Upward
        vPlays3 sqZY sqTO sqZU Downward
        vPlays3 sqNAU sqTAU sqLY Upward
        drops (Kok1,Maun1) sqMU Downward
        drops (Huok2,Kaun1) sqLIA Upward
        drops (Kok1,Uai1) sqTY Downward
        drops (Huok2,Kauk2) sqCU Upward
        vPlays3 sqZU sqXU sqXO Downward
        vPlays2 sqMAI sqMY Upward
        vPlays2 sqME sqCA Downward
        vPlays3 sqLY sqNY sqNAI Upward
        vPlays3 sqNU sqXU sqZI Downward
        drops (Kok1,Kauk2) sqZIA Upward
        vPlays2 sqXO sqCY Downward
        vPlays3 sqTIA sqZIA sqXIA Upward
        vPlays3 sqNAI sqTAU sqTIA Downward
        vPlays3 sqXIA sqZIA sqZU Upward
        vPlays3 sqTY sqTAI sqZAU Downward
        vPlays3 sqTIA sqLAI sqKY Upward
        vPlays2 sqKY sqLU Downward
        vPlays2 sqKAU sqLAU Upward
        vPlays3 sqLU sqLU sqLO Downward
        vPlays3 sqLO sqKO sqKU Upward
        vPlays3 sqLE sqLA sqLAI Downward
        vPlays2 sqXAU sqCIA Upward
        vPlays2 sqLAI sqLI Downward
        vPlays2 sqCU sqCI Upward
        vPlays3 sqXI sqXU sqCU Downward
        vPlays2 sqTAU sqZAI Upward
        vPlays2 sqKU sqKY Downward
        vPlays3 sqNIA sqZAI sqKI Upward
        vPlays2 sqKY sqLY Downward
        vPlays3 sqZU sqZAI sqZAU Upward
        vPlays2 sqLY sqNO Downward
        drops (Kok1,Uai1) sqLAI Upward
        drops (Huok2,Kauk2) sqXIA Downward
        vPlays2 sqPAI sqPY Upward
        drops (Kok1,Nuak1) sqME Downward
        vPlays3 sqNO sqTO sqNI Upward
        vPlays2 sqLI sqLO Downward
        vPlays3 sqCIA sqXIA sqXAU Upward
        vPlays3 sqCU sqMU sqCO Downward
        drops (Huok2,Kauk2) sqNO Upward
        vPlays2 sqCY sqXY Downward
        vPlays3 sqPIA sqPAU sqPAI Upward
        vPlays3 sqXY sqZAI sqZAU Downward
        drops (Kok1,Uai1) sqTAU Upward
        vPlays2 sqMU sqXE Downward
        vPlays3 sqNI sqNI sqLU Upward
        vPlays3 sqLU sqNO sqLY Downward
        vPlays3 sqLY sqNY sqZO Upward
        vPlays3 sqZO sqXU sqMO Downward
        vPlays3 sqLIA sqTAI sqCU Upward
        vPlays3 sqMO sqPY sqPU Downward
        vPlays3 sqCU sqMI sqXA Upward
        vPlays3 sqXU sqCO sqMY Downward
        drops (Kok1,Dau2) sqNU Upward
        vPlays3 sqPU sqMI sqMO Downward
        vPlays3 sqMO sqCAI sqXAI Upward
        vPlays2 sqTE sqNA Downward
        vPlays3 sqXAI sqTAI sqNAI Upward
        vPlays3 sqNAI sqTAI sqLY Downward
        vPlays3 sqXAU sqZAI sqZAU Upward
        vPlays3 sqLY sqLO sqNAI Downward
        drops (Huok2,Uai1) sqXAU Upward
        drops (Kok1,Kauk2) sqLI Downward
        vPlays3 sqTAI sqZAI sqZY Upward
        drops (Huok2,Gua2) sqKY Downward
        vPlays3 sqPAU sqPAI sqMAI Upward
        vPlays3 sqNAI sqZY sqXAI Downward
        vPlays3 sqXAI sqZAI sqZO Upward
        vPlays3 sqZO sqZY sqXAI Downward
        vPlays3 sqXAI sqZY sqNAU Upward
        vPlays3 sqKA sqLA sqLE Downward
        vPlays3 sqNAU sqNY sqTAI Upward
        vPlays3 sqZI sqZY sqXAI Downward
        vPlays3 sqMAI sqCAI sqCIA Upward
        vPlays2 sqLO sqLU Downward
        vPlays3 sqZAI sqCO sqMY Upward
        vPlays3 sqPE sqME sqMA Downward
        vPlays2 sqPY sqPO Upward
        vPlays3 sqXAI sqXE sqXA Downward
        vPlays3 sqTAI sqTO sqZU Upward
        drops (Kok1,Maun1) sqNAU Downward
        vPlays3 sqMY sqCAI sqTU Upward
        vPlays2 sqLU sqKU Downward
        vPlays3 sqXAU sqCAI sqCY Upward
        drops (Huok2,Kaun1) sqTAI Downward
        drops (Huok2,Dau2) sqCE Upward
        vPlays3 sqKU sqKO sqKI Downward
        vPlays3 sqZAU sqTAI sqNAI Upward
        vPlays3 sqCO sqCY sqMAI Downward
        vPlays2 sqZU sqTY Upward
        vPlays3 sqMAI sqMAU sqCAU Downward
        vPlays3 sqTY sqZY sqZAU Upward
        vPlays3 sqCAU sqMAU sqMIA Downward
        vPlays3 sqLAI sqNY sqTO Upward
        vPlays3 sqKY sqNY sqTY Downward
        vPlays3 sqZAU sqZAU sqXAU Upward
        vPlays3 sqTY sqZY sqCY Downward
        vPlays3 sqNAI sqTAI sqNAU Upward
        drops (Huok2,Uai1) sqZA Downward
        vPlays3 sqXAU sqCIA sqZAI Upward
        vPlays3 sqCY sqZY sqPY Downward
        drops (Huok2,Kauk2) sqPIA Upward
        vPlays3 sqZA sqXA sqCE Downward
        declare Downward Saup1
    it "random (new automatic) 027" $
      "_2k^5k - _5k - ^5h_#k_6k_6k\n\
      \^3h^7k -  -  - _!h -  -  - \n\
      \_1h -  -  -  - _1k_8k_1k_1h\n\
      \ - _1k^4h_1h^$h_2h -  - _8k\n\
      \_1h_3k_7h -  - ^5h -  - ^1h\n\
      \ - ^2h -  - ^4h^8h - _1h - \n\
      \^1h - _6h - _1k^1k^4k^1k - \n\
      \^4k - ^3k^1k_3h - ^#h -  - \n\
      \_7k -  - ^1k^!k - _1h^2k - \n\
      \~~~\n\
      \^7h^6h^8h\n" `shouldBeGeneratedBy` do
       vPlays3 sqCI sqMI sqMU Downward
       vPlays3 sqPAU sqPAI sqPY Upward
       vPlays3 sqTA sqTE sqZE Downward
       vPlays3 sqKAU sqKAI sqKY Upward
       vPlays3 sqZA sqXE sqCE Downward
       vPlays3 sqZO sqCAI sqMY Upward
       vPlays2 sqZI sqZAI Downward
       vPlays3 sqMAU sqMAI sqPAU Upward
       vPlays3 sqCA sqXE sqLY Downward
       vPlays2 sqPAU sqMAU Upward
       vPlays3 sqMY sqMU sqMO Downward
       vPlays3 sqNAI sqTAI sqTY Upward
       vPlays3 sqMO sqMAI sqCY Downward
       vPlays3 sqTAU sqZAI sqXY Upward
       vPlays2 sqCY sqZU Downward
       vPlays3 sqLAU sqLAI sqLY Upward
       vPlays3 sqMA sqXI sqTA Downward
       drops (Kok1,Kaun1) sqZAU Upward
       vPlays3 sqKE sqLE sqNE Downward
       vPlays2 sqZU sqZO Upward
       vPlays3 sqZO sqTY sqNO Downward
       vPlays3 sqZAU sqXAI sqCAU Upward
       vPlays3 sqNO sqNO sqLU Downward
       vPlays3 sqTIA sqZIA sqTAU Upward
       vPlays3 sqCE sqME sqMA Downward
       vPlays2 sqLY sqLO Upward
       vPlays2 sqKA sqKE Downward
       vPlays3 sqLU sqLI sqNO Upward
       drops (Huok2,Nuak1) sqCO Downward
       vPlays3 sqLIA sqTAI sqXO Upward
       vPlays3 sqNO sqLI sqKU Downward
       vPlays3 sqKU sqKI sqNO Upward
       vPlays3 sqPE sqPI sqPU Downward
       vPlays3 sqNO sqLAI sqLY Upward
       vPlays3 sqNI sqLI sqLU Downward
       vPlays3 sqXY sqCAI sqTU Upward
       vPlays2 sqXE sqCA Downward
       vPlays3 sqKY sqTY sqTI Upward
       vPlays3 sqLY sqKAI sqKY Downward
       vPlays2 sqKY sqLY Upward
       vPlays3 sqZE sqTE sqTI Downward
       vPlays2 sqTU sqCA Upward
       vPlays3 sqPU sqMU sqCU Downward
       vPlays3 sqLY sqLO sqTO Upward
       vPlays3 sqTO sqTO sqNO Downward
       drops (Huok2,Dau2) sqTO Upward
       vPlays3 sqNA sqLE sqCAI Downward
       vPlays3 sqNO sqLAI sqLAU Upward
       vPlays3 sqCAI sqMAU sqPIA Downward
       vPlays3 sqMIA sqXAI sqMO Upward
       vPlays3 sqLAU sqLAU sqNAU Downward
       vPlays3 sqTAU sqTAI sqZY Upward
       vPlays2 sqMA sqCA Downward
       vPlays3 sqLAI sqLO sqLU Upward
       drops (Huok2,Tuk2) sqLIA Downward
       vPlays2 sqNAU sqTIA Upward
       vPlays3 sqTIA sqTIA sqZAU Downward
       vPlays3 sqZAU sqTAI sqTAU Upward
       vPlays3 sqZAI sqTAI sqNAI Downward
       drops (Kok1,Kauk2) sqTIA Upward
       vPlays3 sqTAU sqXAI sqZAI Downward
       vPlays3 sqCIA sqXAU sqPO Upward
       drops (Huok2,Dau2) sqXY Downward
       drops (Huok2,Kauk2) sqXU Upward
       drops (Huok2,Kua2) sqNI Downward
       vPlays3 sqZY sqTAI sqNAI Upward
       vPlays3 sqKE sqKI sqKY Downward
       vPlays2 sqTAI sqTAU Upward
       vPlays3 sqKY sqKI sqKE Downward
       vPlays3 sqZIA sqTIA sqZAU Upward
       vPlays3 sqTE sqNI sqTU Downward
       vPlays2 sqZAI sqTAI Upward
       vPlays3 sqTAI sqXAI sqCY Downward
       vPlays3 sqCY sqXY sqZAI Upward
       vPlays3 sqNI sqNAI sqLAI Downward
       vPlays3 sqXO sqTI sqLA Upward
       vPlays2 sqME sqCE Downward
       vPlays2 sqTO sqZY Upward
       vPlays3 sqLAI sqLO sqLAU Downward
       vPlays3 sqZAI sqZY sqTO Upward
       vPlays3 sqCE sqCA sqCI Downward
       vPlays2 sqLO sqLY Upward
       vPlays3 sqTO sqXY sqXO Downward
       vPlays2 sqXAU sqCAI Upward
       vPlays3 sqCU sqTU sqNO Downward
       vPlays3 sqMAU sqCAU sqCIA Upward
       vPlays3 sqXO sqXU sqZI Downward
       vPlays3 sqZI sqXI sqTO Upward
       vPlays3 sqXY sqTAU sqNAI Downward
       vPlays2 sqCIA sqMIA Upward
       vPlays2 sqXA sqCE Downward
       drops (Huok2,Maun1) sqXA Upward
       vPlays3 sqTI sqNE sqTE Downward
       vPlays3 sqTO sqNO sqLAI Upward
       vPlays3 sqNAI sqTAU sqZIA Downward
       vPlays3 sqXIA sqZIA sqXAU Upward
       vPlays3 sqLIA sqLAU sqKAU Downward
       vPlays3 sqLAI sqLY sqNAI Upward
       vPlays3 sqKAU sqLAU sqNAU Downward
       vPlays3 sqNAI sqLY sqLAI Upward
       vPlays2 sqTE sqZE Downward
       vPlays3 sqPO sqCAI sqTU Upward
       vPlays3 sqLAI sqLY sqKAU Downward
       vPlays3 sqNIA sqLAU sqLAI Upward
       drops (Huok2,Kauk2) sqKO Downward
       vPlays3 sqZAU sqXAU sqZIA Upward
       vPlays3 sqKAU sqKAI sqLIA Downward
       vPlays3 sqLIA sqNAU sqZAI Upward
       vPlays2 sqCI sqXU Downward
       vPlays3 sqZIA sqXAU sqXIA Upward
       vPlays3 sqLE sqLA sqKA Downward
       vPlays3 sqZAI sqCAI sqMY Upward
       vPlays2 sqMY sqXO Downward
       vPlays3 sqLAI sqXI sqZE Upward
       vPlays2 sqLI sqLU Downward
       vPlays3 sqXO sqZY sqZO Upward
       vPlays2 sqZO sqZU Downward
       drops (Huok2,Dau2) sqTO Upward
       vPlays3 sqPIA sqCAI sqCAU Downward
       vPlays2 sqZE sqKY Upward
       drops (Kok1,Kauk2) sqZAI Downward
       vPlays2 sqTO sqNU Upward
       vPlays3 sqCO sqCAI sqCI Downward
       vPlays3 sqPAI sqPY sqPO Upward
       vPlays2 sqZU sqTI Downward
       drops (Kok1,Nuak1) sqZIA Upward
       drops (Kok1,Kaun1) sqNI Downward
       drops (Kok1,Dau2) sqKAU Upward
       vPlays3 sqNI sqNU sqLO Downward
       vPlays2 sqTU sqZU Upward
       vPlays3 sqNE sqTI sqTY Downward
       vPlays3 sqZU sqXI sqZE Upward
       vPlays3 sqLAU sqNAU sqNAI Downward
       vPlays3 sqTI sqNU sqTO Upward
       vPlays3 sqMU sqMO sqMY Downward
       vPlays3 sqTO sqXU sqZI Upward
       vPlays3 sqCI sqXI sqXE Downward
       vPlays3 sqMO sqXI sqXO Upward
       vPlays3 sqZI sqXU sqCY Downward
       vPlays3 sqCY sqXO sqZU Upward
       vPlays3 sqTY sqXU sqKIA Downward
       vPlays3 sqZE sqNU sqKE Upward
       drops (Kok1,Uai1) sqPU Downward
       vPlays3 sqPY sqPO sqMO Upward
       vPlays3 sqCE sqMI sqCI Downward
       vPlays3 sqMO sqMY sqCY Upward
       drops (Huok2,Kauk2) sqTU Downward
       vPlays3 sqZU sqXI sqZE Upward
       drops (Huok2,Kauk2) sqCIA Downward
       vPlays3 sqXAU sqCAI sqXY Upward
       vPlays3 sqCAU sqXAI sqZAU Downward
       vPlays2 sqXIA sqCAU Upward
       drops (Kok1,Kua2) sqMA Downward
       vPlays3 sqCY sqXY sqLE Upward
       vPlays3 sqZE sqXU sqZU Downward
       vPlays2 sqKY sqNAU Upward
       declare Upward Huep2Hia1
    it "random (new automatic) 041" $
      " - _5h - ^3h^7k - _3k^5h - \n\
      \ -  - ^!k -  -  -  -  - _6k\n\
      \_1h_6h - _1k^4k - _1h_1k_1h\n\
      \ -  -  - ^$h - ^1k_#k -  - \n\
      \ -  -  - _8k -  -  - _5k - \n\
      \^4k -  - ^4h -  -  - ^1k^6h\n\
      \_2k^1k^1h_8h - ^1k -  - _7h\n\
      \^7h^2h^#h -  - ^4h - ^2k - \n\
      \^6k^5k^3k^8k^!h^8h - _2h - \n\
      \~~~\n\
      \_1h^1h_1k_1h^7k_1h_1k_3h\n" `shouldBeGeneratedBy` do
       vPlays3 sqZO sqZO sqXU Downward
       vPlays3 sqXAU sqZAI sqXY Upward
       vPlays3 sqXU sqMI sqMU Downward
       vPlays3 sqCAI sqMAI sqMY Upward
       vPlays3 sqMA sqXI sqMO Downward
       vPlays3 sqMIA sqXAI sqTO Upward
       vPlays3 sqPE sqPI sqPO Downward
       vPlays3 sqMU sqPI sqPE Upward
       vPlays3 sqZA sqTA sqZE Downward
       vPlays3 sqXY sqNI sqTE Upward
       vPlays2 sqPO sqPY Downward
       drops (Kok1,Dau2) sqKY Upward
       vPlays3 sqZE sqXI sqCU Downward
       vPlays3 sqZIA sqTAU sqNAU Upward
       vPlays3 sqME sqMI sqMU Downward
       vPlays3 sqCIA sqMAU sqNI Upward
       vPlays3 sqPE sqCA sqMA Downward
       vPlays2 sqTO sqLI Upward
       vPlays3 sqMA sqPA sqPE Downward
       drops (Huok2,Kauk2) sqTO Upward
       vPlays3 sqTA sqTE sqNI Downward
       vPlays2 sqTE sqZI Upward
       vPlays3 sqNA sqZI sqNO Downward
       drops (Kok1,Kauk2) sqXO Upward
       vPlays3 sqPE sqCI sqXU Downward
       vPlays3 sqXU sqTO sqZO Upward
       vPlays3 sqNI sqTI sqZE Downward
       vPlays3 sqZO sqTAI sqZAU Upward
       vPlays3 sqZAU sqZAU sqXAU Downward
       drops (Kok1,Nuak1) sqNE Upward
       vPlays3 sqXAU sqMY sqCY Downward
       vPlays3 sqCY sqMAI sqMIA Upward
       vPlays3 sqMU sqCU sqCIA Downward
       vPlays3 sqLI sqTO sqXI Upward
       vPlays3 sqKE sqKI sqKU Downward
       vPlays3 sqNAU sqNAI sqNY Upward
       vPlays3 sqLE sqNE sqNU Downward
       drops (Kok1,Kauk2) sqCAI Upward
       vPlays3 sqNO sqZAI sqXY Downward
       vPlays3 sqPAU sqXO sqXY Upward
       vPlays2 sqXE sqZA Downward
       vPlays2 sqCAI sqCO Upward
       vPlays3 sqXA sqZE sqXE Downward
       vPlays2 sqXI sqMA Upward
       vPlays3 sqCU sqMO sqPU Downward
       vPlays2 sqXO sqXU Upward
       vPlays3 sqCIA sqMIA sqMAU Downward
       vPlays3 sqNY sqLAI sqNAU Upward
       drops (Kok1,Gua2) sqCU Downward
       vPlays2 sqMIA sqCIA Upward
       vPlays2 sqMAU sqMIA Downward
       vPlays3 sqXY sqKY sqKO Upward
       vPlays2 sqPY sqPAI Downward
       drops (Huok2,Kaun1) sqTA Upward
       vPlays3 sqKA sqLA sqLE Downward
       vPlays2 sqCIA sqZAU Upward
       vPlays3 sqZAU sqTIA sqZIA Downward
       vPlays3 sqTAU sqNAI sqLY Upward
       vPlays3 sqXE sqZI sqZU Downward
       vPlays2 sqCO sqCU Upward
       vPlays2 sqPA sqPE Downward
       vPlays3 sqLY sqNAI sqTY Upward
       vPlays2 sqLE sqLU Downward
       vPlays2 sqZIA sqCAU Upward
       drops (Huok2,Kauk2) sqMAU Downward
       vPlays3 sqCAU sqXAI sqXAU Upward
       vPlays3 sqXAU sqZAI sqTAU Downward
       drops (Kok1,Gua2) sqCY Upward
       vPlays3 sqPU sqMO sqCU Downward
       vPlays2 sqZAI sqZIA Upward
       vPlays2 sqLU sqLI Downward
       vPlays3 sqTAU sqNAI sqLO Upward
       vPlays3 sqZE sqTI sqNI Downward
       vPlays2 sqKO sqNO Upward
       vPlays3 sqKU sqZU sqZO Downward
       vPlays3 sqLO sqTO sqTU Upward
       vPlays3 sqNU sqTU sqLU Downward
       vPlays3 sqPIA sqPAI sqPY Upward
       vPlays3 sqZO sqTY sqMY Downward
       vPlays2 sqMAI sqMY Upward
       vPlays3 sqZU sqTU sqTO Downward
       vPlays3 sqNO sqZI sqZA Upward
       vPlays3 sqNI sqTU sqNO Downward
       drops (Huok2,Dau2) sqXAU Upward
       vPlays3 sqNO sqTY sqTAI Downward
       vPlays3 sqCY sqMY sqMAU Upward
       vPlays3 sqLU sqLAI sqKAI Downward
       declare Downward HuetKaikADat2
    it "random (new automatic) 143" $
      "_6h - _1h_1h^!h - ^4h -  - \n\
      \_7k_#k -  -  -  -  -  - ^1k\n\
      \_1h -  - _8h -  -  - _1k - \n\
      \ -  -  - _4h_2k^3h^1k_6k_1h\n\
      \_3h^5k -  -  -  - ^8k_5k^5h\n\
      \ -  - ^8h_7k_1h - ^2h - ^1h\n\
      \^2k - _4k - _7h_1k -  -  - \n\
      \_3k^$h -  - ^1h^1k - _1k - \n\
      \^6k_1h - _1k - _1k^5h^2h - \n\
      \~~~\n\
      \_#h_3k_7h^8k^6h_!k_4k\n" `shouldBeGeneratedBy` do
       vPlays3 sqTAU sqNAI sqKO Upward
       vPlays3 sqXA sqZA sqZE Downward
       vPlays3 sqXIA sqXAU sqCAU Upward
       vPlays3 sqZE sqXI sqZU Downward
       vPlays3 sqXAU sqZAI sqXY Upward
       vPlays3 sqZO sqZU sqCU Downward
       vPlays3 sqCU sqZI sqZE Upward
       vPlays2 sqTA sqNE Downward
       vPlays2 sqZIA sqZAU Upward
       vPlays3 sqZI sqTI sqTY Downward
       vPlays3 sqZE sqXE sqTA Upward
       vPlays3 sqZU sqTI sqNU Downward
       vPlays3 sqTA sqZA sqXA Upward
       vPlays3 sqME sqXE sqZI Downward
       vPlays3 sqTAI sqTY sqTO Upward
       vPlays3 sqXA sqXE sqTA Downward
       vPlays3 sqZAU sqTIA sqZIA Upward
       vPlays3 sqZA sqXE sqCE Downward
       vPlays3 sqLAU sqLAI sqLY Upward
       vPlays3 sqTA sqTE sqZE Downward
       vPlays2 sqCAI sqCY Upward
       vPlays3 sqZI sqTE sqZA Downward
       vPlays3 sqZAI sqZE sqZA Upward
       vPlays3 sqLA sqTI sqXO Downward
       drops (Huok2,Gua2) sqTA Upward
       vPlays3 sqNE sqLI sqKU Downward
       vPlays3 sqTA sqNA sqLA Upward
       vPlays3 sqZE sqTI sqXU Downward
       vPlays3 sqLY sqTY sqTAI Upward
       vPlays3 sqXU sqMI sqPU Downward
       vPlays3 sqLA sqNA sqNI Upward
       vPlays2 sqCI sqCU Downward
       vPlays3 sqPU sqPU sqMO Upward
       vPlays3 sqLE sqLI sqNI Downward
       drops (Huok2,Kauk2) sqCI Upward
       drops (Huok2,Gua2) sqPU Downward
       vPlays3 sqMO sqPU sqMY Upward
       vPlays3 sqPU sqPAI sqMAU Downward
       vPlays3 sqXY sqNIA sqLAU Upward
       vPlays3 sqXE sqCI sqPO Downward
       vPlays2 sqCIA sqXAU Upward
       vPlays2 sqMY sqXU Downward
       vPlays3 sqZIA sqXAU sqCAI Upward
       vPlays3 sqCE sqXI sqZU Downward
       vPlays3 sqMIA sqXAI sqMO Upward
       drops (Kok1,Gua2) sqXY Downward
       vPlays3 sqMO sqXAI sqMIA Upward
       vPlays2 sqPI sqPU Downward
       vPlays3 sqCAI sqCY sqXO Upward
       vPlays2 sqNU sqTU Downward
       vPlays3 sqTAI sqTY sqNY Upward
       vPlays2 sqXU sqZE Downward
       vPlays3 sqZE sqTI sqXA Upward
       vPlays3 sqXA sqCA sqME Downward
       vPlays3 sqME sqPA sqPI Upward
       vPlays3 sqZU sqTO sqNU Downward
       vPlays3 sqTO sqTU sqZU Upward
       vPlays2 sqNI sqLU Downward
       vPlays2 sqKAI sqKY Upward
       vPlays3 sqPI sqPO sqMY Downward
       drops (Huok2,Maun1) sqZAI Upward
       vPlays3 sqMY sqXY sqZO Downward
       vPlays3 sqXAU sqZAI sqCIA Upward
       vPlays2 sqPE sqME Downward
       vPlays2 sqCY sqCO Upward
       vPlays3 sqZO sqTU sqNO Downward
       vPlays3 sqZAI sqNO sqZO Upward
       vPlays2 sqXI sqXU Downward
       vPlays3 sqNY sqLAI sqLO Upward
       vPlays3 sqNO sqNU sqNI Downward
       vPlays2 sqNAI sqNY Upward
       vPlays3 sqXY sqCO sqMO Downward
       vPlays3 sqNIA sqCO sqMY Upward
       vPlays3 sqNI sqTU sqLY Downward
       vPlays3 sqNY sqTY sqTAI Upward
       vPlays3 sqTE sqZA sqXE Downward
       vPlays3 sqLO sqZO sqZAI Upward
       vPlays3 sqTU sqZO sqTO Downward
       vPlays2 sqKO sqLU Upward
       vPlays3 sqLY sqLAI sqTAU Downward
       drops (Kok1,Gua2) sqTU Upward
       vPlays3 sqTY sqTAI sqNAI Downward
       vPlays2 sqXAI sqXY Upward
       vPlays2 sqTO sqNY Downward
       vPlays2 sqLU sqKO Upward
       vPlays2 sqMAU sqMIA Downward
       vPlays3 sqZAI sqZO sqXU Upward
       vPlays2 sqNY sqLAI Downward
       drops (Kok1,Kauk2) sqPE Upward
       vPlays3 sqTAU sqLAI sqLY Downward
       vPlays2 sqTU sqZI Upward
       vPlays3 sqMIA sqMAI sqMY Downward
       vPlays3 sqZI sqTI sqLI Upward
       vPlays3 sqLY sqNU sqNO Downward
       vPlays3 sqZO sqNAI sqZAI Upward
       vPlays3 sqNO sqTAI sqZY Downward
       vPlays3 sqXO sqXY sqZO Upward
       drops (Kok1,Kaun1) sqLA Downward
       vPlays3 sqZAI sqZO sqCAI Upward
       vPlays2 sqKU sqLI Downward
       vPlays2 sqCI sqXI Upward
       drops (Kok1,Gua2) sqTY Downward
       vPlays3 sqZO sqZY sqTY Upward
       vPlays3 sqMO sqMY sqMU Downward
       vPlays2 sqTY sqTO Upward
       vPlays3 sqNAI sqTAI sqTAU Downward
       vPlays2 sqMAI sqMY Upward
       vPlays3 sqNA sqKI sqNO Downward
       drops (Huok2,Gua2) sqXAU Upward
       drops (Kok1,Kauk2) sqCY Downward
       vPlays3 sqXU sqZU sqZO Upward
       vPlays3 sqLA sqTI sqZE Downward
       vPlays3 sqZO sqTO sqTI Upward
       vPlays3 sqZY sqTAI sqNAU Downward
       drops (Kok1,Kauk2) sqCE Upward
       vPlays2 sqNAU sqLY Downward
       drops (Kok1,Kauk2) sqZAI Upward
       vPlays3 sqNO sqLY sqNAI Downward
       drops (Kok1,Gua2) sqNI Upward
       vPlays3 sqNAI sqLAI sqNAU Downward
       vPlays2 sqZU sqZI Upward
       vPlays3 sqLY sqKO sqLO Downward
       vPlays2 sqCAU sqXAI Upward
       vPlays3 sqLO sqNU sqZO Downward
       vPlays3 sqXAI sqZAI sqZY Upward
       vPlays3 sqNU sqLI sqNE Downward
       vPlays2 sqNI sqNY Upward
       vPlays2 sqNE sqTA Downward
       vPlays3 sqZO sqCO sqXO Upward
       drops (Huok2,Maun1) sqPI Downward
       vPlays3 sqXAU sqPAU sqTAU Upward
       vPlays3 sqXO sqXY sqZO Downward
       vPlays3 sqZY sqTAI sqNAU Upward
       vPlays3 sqZO sqTO sqXO Downward
       vPlays3 sqXO sqXY sqTY Upward
       vPlays3 sqLAI sqNY sqLO Downward
       vPlays3 sqPAU sqPAI sqPY Upward
       vPlays3 sqTY sqNY sqZO Downward
       drops (Huok2,Kaun1) sqXA Upward
       vPlays2 sqZO sqNAI Downward
       vPlays3 sqTI sqTO sqTY Upward
       vPlays3 sqNAI sqLO sqNU Downward
       vPlays3 sqNU sqNU sqTU Upward
       vPlays2 sqMA sqXI Downward
       vPlays2 sqTY sqZY Upward
       drops (Huok2,Kauk2) sqNA Downward
       vPlays3 sqCIA sqZAI sqLU Upward
       vPlays2 sqTU sqXO Downward
       vPlays3 sqXO sqTO sqNU Upward
       vPlays2 sqXI sqMO Downward
       vPlays2 sqLU sqNI Upward
       vPlays3 sqNU sqTO sqXI Downward
       vPlays3 sqXI sqCU sqXO Upward
       vPlays3 sqXO sqXY sqXU Downward
       vPlays3 sqNY sqZY sqZO Upward
       vPlays2 sqLO sqKO Downward
       vPlays2 sqNI sqTU Upward
       drops (Huok2,Dau2) sqLE Downward
       vPlays3 sqZO sqZI sqTE Upward
       vPlays3 sqXU sqXU sqZO Downward
       vPlays2 sqCAI sqCIA Upward
       vPlays3 sqTA sqZA sqTE Downward
       vPlays3 sqNAU sqTAU sqZAU Upward
       drops (Kok1,Gua2) sqXIA Downward
       vPlays3 sqTU sqZI sqXU Upward
       vPlays3 sqTE sqNA sqLA Downward
       vPlays3 sqTAU sqLAU sqLO Upward
       vPlays3 sqXIA sqXY sqZAI Downward
       vPlays3 sqZAU sqTAI sqNY Upward
       vPlays3 sqZAI sqPAI sqPIA Downward
       vPlays3 sqLO sqLAU sqCAU Upward
       vPlays3 sqZO sqNY sqNAI Downward
       vPlays2 sqZY sqZU Upward
       vPlays2 sqPIA sqMIA Downward
       vPlays3 sqCAU sqLAU sqPAU Upward
       drops (Kok1,Kauk2) sqKAI Downward
       vPlays3 sqXA sqZE sqLO Upward
       drops (Huok2,Kua2) sqNI Downward
       vPlays2 sqZU sqZY Upward
       vPlays3 sqMU sqCU sqCE Downward
       vPlays3 sqNAI sqZY sqXO Upward
       vPlays3 sqXO sqZI sqTI Downward
       vPlays3 sqZI sqZE sqXE Upward
       vPlays3 sqNI sqTI sqTA Downward
       drops (Kok1,Nuak1) sqNAU Upward
       vPlays3 sqTI sqTI sqZI Downward
       vPlays3 sqXU sqZI sqNO Upward
       drops (Kok1,Kauk2) sqMAU Downward
       vPlays2 sqNO sqLU Upward
       vPlays3 sqZE sqZI sqZU Downward
       vPlays2 sqZI sqNO Upward
       vPlays3 sqMIA sqCIA sqCAI Downward
       vPlays3 sqTIA sqNAU sqNAI Upward
       vPlays3 sqCY sqCAI sqXAI Downward
       vPlays3 sqZY sqXY sqCAI Upward
       vPlays3 sqCE sqCU sqXU Downward
       vPlays3 sqLU sqLO sqNU Upward
       vPlays3 sqXU sqZU sqZAI Downward
       vPlays3 sqNO sqLO sqTU Upward
       vPlays3 sqKO sqKY sqLO Downward
       drops (Kok1,Dau2) sqNO Upward
       vPlays3 sqTA sqZA sqZI Downward
       vPlays2 sqTU sqTE Upward
       vPlays3 sqCA sqXE sqCI Downward
       vPlays2 sqPY sqPO Upward
       vPlays2 sqLI sqNI Downward
       vPlays3 sqTO sqNY sqLAI Upward
       vPlays3 sqZI sqZU sqTU Downward
       vPlays3 sqTE sqZA sqTI Upward
       vPlays2 sqTI sqTA Downward
       vPlays3 sqLAI sqKY sqLY Upward
       vPlays3 sqLO sqLY sqNAI Downward
       vPlays3 sqLAU sqNAI sqCI Upward
       drops (Huok2,Kaun1) sqTI Downward
       drops (Kok1,Gua2) sqTY Upward
       vPlays2 sqNAI sqTAI Downward
       drops (Huok2,Dau2) sqCA Upward
       vPlays3 sqNI sqTU sqNU Downward
       vPlays2 sqLY sqKO Upward
       drops (Kok1,Uai1) sqZO Downward
       drops (Kok1,Kaun1) sqZIA Upward
       drops (Huok2,Kaun1) sqCY Downward
       vPlays3 sqNO sqTY sqLU Upward
       vPlays2 sqZU sqXI Downward
       vPlays2 sqNY sqLAI Upward
       vPlays3 sqTA sqZA sqCE Downward
       vPlays2 sqCAI sqXAU Upward
       vPlays3 sqXI sqXE sqXA Downward
       vPlays3 sqCE sqCE sqMA Upward
       vPlays3 sqME sqPE sqXE Downward
       vPlays3 sqMA sqMI sqME Upward
       vPlays2 sqNU sqLI Downward
       vPlays3 sqME sqCI sqMA Upward
       vPlays3 sqTI sqXA sqKY Downward
       vPlays2 sqTY sqNO Upward
       drops (Huok2,Kauk2) sqTA Downward
       vPlays2 sqLIA sqTAI Upward
       drops (Huok2,Kauk2) sqLIA Downward
       vPlays3 sqMA sqXE sqZI Upward
       vPlays3 sqXE sqTU sqTAU Downward
       vPlays2 sqZI sqZE Upward
       vPlays3 sqPA sqPE sqME Downward
       vPlays3 sqPO sqPU sqPI Upward
       vPlays2 sqTU sqXU Downward
       vPlays3 sqZE sqXA sqZI Upward
       vPlays3 sqZO sqXU sqCO Downward
       vPlays2 sqTAI sqLO Upward
       vPlays3 sqCY sqXAI sqCAU Downward
       vPlays3 sqNO sqNA sqNAI Upward
       vPlays3 sqZAI sqZI sqTE Downward
       vPlays2 sqXAU sqCAU Upward
       vPlays3 sqTE sqLE sqCE Downward
       vPlays2 sqLAI sqLY Upward
       vPlays2 sqZI sqNI Downward
       vPlays3 sqNI sqLE sqTI Upward
       vPlays3 sqXA sqCE sqLAI Downward
       drops (Huok2,Kaun1) sqZAI Upward
       vPlays2 sqME sqMA Downward
       vPlays2 sqCI sqMU Upward
       vPlays3 sqMA sqMI sqMU Downward
       vPlays3 sqTI sqZA sqZE Upward
       drops (Kok1,Kauk2) sqXIA Downward
       vPlays2 sqPAI sqPY Upward
       vPlays2 sqZE sqNE Downward
       vPlays3 sqCAU sqCIA sqCAI Upward
       drops (Huok2,Kauk2) sqZY Downward
       vPlays3 sqNAI sqLAI sqKAI Upward
       vPlays2 sqLE sqNI Downward
       drops (Kok1,Uai1) sqXO Upward
       vPlays3 sqCE sqCA sqXA Downward
       vPlays2 sqLY sqNY Upward
       vPlays3 sqLI sqNI sqTE Downward
       drops (Huok2,Maun1) sqCI Upward
       drops (Kok1,Dau2) sqNAI Downward
       drops (Kok1,Kauk2) sqXAU Upward
       vPlays2 sqCO sqMY Downward
       vPlays3 sqPI sqPU sqPO Upward
       vPlays3 sqNE sqNI sqNU Downward
       vPlays3 sqCAI sqCIA sqMIA Upward
       vPlays3 sqLA sqKA sqLE Downward
       vPlays2 sqXO sqCO Upward
       vPlays2 sqMY sqPO Downward
       vPlays2 sqZAI sqTAU Upward
       vPlays2 sqNI sqTU Downward
       vPlays3 sqMIA sqMAU sqCAU Upward
       vPlays3 sqLAI sqNY sqKAU Downward
       vPlays2 sqPAU sqPIA Upward
       drops (Kok1,Tuk2) sqNIA Downward
       drops (Huok2,Tuk2) sqZO Upward
       vPlays3 sqKY sqLO sqLU Downward
       vPlays2 sqZO sqTY Upward
       vPlays3 sqNU sqTU sqLY Downward
       vPlays2 sqTY sqTI Upward
       vPlays3 sqXA sqZA sqZU Downward
       vPlays3 sqZIA sqXAU sqTY Upward
       drops (Kok1,Kauk2) sqTIA Downward
       vPlays3 sqXY sqXU sqCU Upward
       vPlays3 sqLY sqKAU sqLAU Downward
       vPlays3 sqPIA sqPY sqCY Upward
       vPlays3 sqNIA sqLIA sqNAU Downward
       vPlays3 sqTAU sqCO sqXU Upward
       drops (Huok2,Tuk2) sqZAI Downward
       vPlays2 sqCI sqPO Upward
       vPlays2 sqTE sqTI Downward
       vPlays3 sqCAU sqMAU sqMIA Upward
       vPlays3 sqNAU sqNAI sqTY Downward
       drops (Huok2,Kauk2) sqZAU Upward
       vPlays3 sqLU sqZA sqKO Downward
       declare Downward Mok1Mok1
       declare Downward Dat2AIo
       declare Downward Saup1






      


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


