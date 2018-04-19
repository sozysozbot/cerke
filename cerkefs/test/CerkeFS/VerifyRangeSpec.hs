module CerkeFS.VerifyRangeSpec (spec) where

import Test.Hspec
import CerkeFS
import CerkeFS.Operations
import CerkeFS.TestUtilities

spec :: Spec
spec = do
  describe "error detections" $ do
    it "fedirrgavir 000 with validator" $ 
      ProfessionPrivilegeExceeded Dau2 sqTY `shouldBeThrownBy` fed000'
  describe "correct behaviors" $ do
    it "fedirrgavir 001 with validator" $ res001 `shouldBeGeneratedBy` fed001'
    it "fedirrgavir 002 with validator" $ res002 `shouldBeGeneratedBy` fed002'
    it "fedirrgavir 003 with validator" $ res003 `shouldBeGeneratedBy` fed003'
    it "fedirrgavir 004 with validator" $ res004 `shouldBeGeneratedBy` fed004'
    it "fedirrgavir 005 with validator" $ res005 `shouldBeGeneratedBy` fed005'


fed000' :: Operation ()
fed000' = do
 vPlays3'      sqTAU 虎 sqNAI sqTY  >+> vPlays2' sqNI 兵 sqNU
 mun1(vPlays3' sqLIA 馬 sqTAI sqXO) >+> vPlays3' sqMA 馬 sqXI sqTO
 vPlays2'      sqTY  虎       sqTO  >+> vPlays3' sqTA 将 sqTE sqNI
 vPlays3'      sqLAU 弓 sqLAI sqLO  >+> vPlays2' sqZA 王 sqZE
 drops'              馬       sqKY  >+> mun1(vPlays2' sqNU 兵 sqNO)
 vPlays3'      sqKY  馬 sqNU  sqZE  >+> vPlays2' sqXA 将 sqZE
 return ()


fed001' :: Operation ()
fed001' = do
 vPlays3'      sqTAU 虎 sqNAI sqTY  >+> vPlays3'      sqXE 虎 sqCI sqXU
 vPlays3'      sqLIA 馬 sqTAI sqXO  >+> mun1(vPlays3' sqXI 兵 sqXU sqXO)
 vPlays2'      sqZAI 船 sqZY  >+> vPlays2'      sqME 弓 sqZE
 mun1(vPlays3' sqXIA 将 sqXAU sqZAI)>+> playsTam    sqZO    sqZAI
 vPlays2'      sqCAI 兵 sqCY  >+> mun1(vPlays3' sqXI 兵 sqXU sqXO) 
 vPlays3'      sqXIA 将 sqXAU sqCAI >+> mun1(vPlays3' sqXI 兵 sqXU sqXO) 
 vPlays2'      sqCAI 将 sqXY  >+> vPlays2'      sqXU 虎 sqTY
 vPlays2'      sqTAI 兵 sqTY  >+> vPlays3'      sqTE 虎 sqZI sqTU
 vPlays3'      sqXY  将 sqXAI sqCAU >+> vPlays2'      sqTU 虎 sqMAU
 vPlays2'      sqCIA 車 sqMAU >+> playsTam    sqZAI   sqXY
 vPlays2'      sqCAU 将 sqCAI >+> vPlays2'      sqZI 船 sqZY
 playsTam      sqXY     sqZAU >+> vPlays3'      sqKE 巫 sqLE sqNE
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

fed002' :: Operation ()
fed002' = do
 vPlays3' sqKE  Tuk2 sqLE sqNE >-> vPlays2' sqTAI Kauk2 sqTY
 vPlays2' sqNI  Kauk2 sqNU >-> playsTam sqZO        sqZAU
 vPlays2' sqNE  Tuk2  sqNI >-> vPlays2' sqNAI Kauk2 sqNY
 vPlays2' sqNI  Tuk2  sqTU >-> vPlays3' sqKAU Tuk2  sqLAU sqNAU
 vPlays3' sqTA  Uai1  sqTE sqNI >-> vPlays2' sqXAI Kauk2 sqXY
 vPlays3' sqTE  Dau2  sqNI sqLU >-> vPlays2' sqNAU Tuk2 sqNAI
 vPlays3' sqXE  Dau2  sqCI sqXU >-> vPlays2' sqCAI Kauk2 sqCY
 vPlays3' sqPE  Tuk2  sqME sqCE >-> vPlays3' sqXIA Uai1 sqXAU sqXAI
 vPlays2' sqCI  Kauk2 sqCU >-> vPlays3' sqTIA Uai1 sqTAU sqTAI
 vPlays3' sqXA  Uai1  sqCE sqCI >-> vPlays3' sqTAU Dau2 sqNAI sqLY
 vPlays3' sqMA  Maun1 sqXI sqTO >-> vPlays2' sqTY  Kauk2 sqTO
 playsTam  sqZAU       sqZO >-> playsTam sqZO        sqZY
 vPlays2' sqLE  Gua2  sqZE >-> vPlays3' sqZAI Nuak1 sqXAI sqCAI
 vPlays3' sqLA  Maun1 sqTI sqLO >-> mun1 (vPlays2' sqXAI Uai1 sqZAI)
 vPlays3' sqKA  Kua2  sqKI sqKU >-> vPlays3' sqLAI Kauk2 sqLY sqLO
 vPlays3' sqLI  Kauk2 sqLU sqLO >-> vPlays3' sqLY  Dau2  sqNAI sqTY
 mun1 (vPlays3' sqZE Gua2 sqZI sqZU) >-> vPlays3' sqZIA Io sqXAU sqCAU
 playsTam  sqZY        sqCO >-> playsTam sqCO        sqZAU
 vPlays2' sqZI  Nuak1 sqZO >-> vPlays2' sqTY Dau2   sqZO
 mun1 (vPlays2' sqZE Gua2 sqZO) >-> vPlays3' sqZO Dau2 sqTU sqNI
 vPlays2' sqTU  Tuk2  sqNI >-> vPlays3' sqTAI Uai1 sqNY sqLO
 vPlays3' sqLU  Dau2  sqNI sqTE >-> vPlays2' sqNAI Tuk2  sqCI
 vPlays2' sqCE  Tuk2  sqCI >-> vPlays2' sqXY  Kauk2 sqXU
 declare Upward Saup1 
 taxot1 

fed002'' :: Operation ()
fed002'' = do
 vPlays3' sqKE  Tuk2 sqLE sqNE >-> vPlays2' sqTAI Kauk2 sqTY
 vPlays2' sqNI  Kauk2 sqNU >-> vPlTam3 sqZO    sqTY    sqZAU
 vPlays2' sqNE  Tuk2  sqNI >-> vPlays2' sqNAI Kauk2 sqNY
 vPlays2' sqNI  Tuk2  sqTU >-> vPlays3' sqKAU Tuk2  sqLAU sqNAU
 vPlays3' sqTA  Uai1  sqTE sqNI >-> vPlays2' sqXAI Kauk2 sqXY
 vPlays3' sqTE  Dau2  sqNI sqLU >-> vPlays2' sqNAU Tuk2 sqNAI
 vPlays3' sqXE  Dau2  sqCI sqXU >-> vPlays2' sqCAI Kauk2 sqCY
 vPlays3' sqPE  Tuk2  sqME sqCE >-> vPlays3' sqXIA Uai1 sqXAU sqXAI
 vPlays2' sqCI  Kauk2 sqCU >-> vPlays3' sqTIA Uai1 sqTAU sqTAI
 vPlays3' sqXA  Uai1  sqCE sqCI >-> vPlays3' sqTAU Dau2 sqNAI sqLY
 vPlays3' sqMA  Maun1 sqXI sqTO >-> vPlays2' sqTY  Kauk2 sqTO
 vPlTam3  sqZAU   sqTAI    sqZO >-> vPlTam3 sqZO  sqXU      sqZY
 vPlays2' sqLE  Gua2  sqZE >-> vPlays3' sqZAI Nuak1 sqXAI sqCAI
 vPlays3' sqLA  Maun1 sqTI sqLO >-> mun1 (vPlays2' sqXAI Uai1 sqZAI)
 vPlays3' sqKA  Kua2  sqKI sqKU >-> vPlays3' sqLAI Kauk2 sqLY sqLO
 vPlays3' sqLI  Kauk2 sqLU sqLO >-> vPlays3' sqLY  Dau2  sqNAI sqTY
 mun1 (vPlays3' sqZE Gua2 sqZI sqZU) >-> vPlays3' sqZIA Io sqXAU sqCAU
 playsTam  sqZY        sqCO >-> vPlTam3 sqCO   sqXY     sqZAU
 vPlays2' sqZI  Nuak1 sqZO >-> vPlays2' sqTY Dau2   sqZO
 mun1 (vPlays2' sqZE Gua2 sqZO) >-> vPlays3' sqZO Dau2 sqTU sqNI
 vPlays2' sqTU  Tuk2  sqNI >-> vPlays3' sqTAI Uai1 sqNY sqLO
 vPlays3' sqLU  Dau2  sqNI sqTE >-> vPlays2' sqNAI Tuk2  sqCI
 vPlays2' sqCE  Tuk2  sqCI >-> vPlays2' sqXY  Kauk2 sqXU
 declare Upward Saup1 
 taxot1 


fed003' :: Operation ()
fed003' = do
 vPlays2'   sqXIA Uai1  sqZAU >+> vPlays2' sqTI Kauk2 sqTU
 playsTam   sqZO        sqCY  >+> vPlays2' sqXI Kauk2 sqXU
 vPlays2'   sqMAI Kauk2 sqMY  >+> vPlays2' sqNI Kauk2 sqNU
 playsTam   sqCY        sqCAU >+> vPlays3' sqKE Tuk2  sqLE sqNE
 vPlays2'   sqCAI Kauk2 sqCY  >+> vPlays2' sqNE Tuk2  sqNI
 vPlays2'   sqZAI Nuak1 sqZY  >+> vPlays2' sqZI Nuak1 sqZY
 vPlays3'   sqZAU Uai1  sqTAI sqZY  >+> vPlays3' sqLE Gua2 sqTE sqZE
 vPlays3'   sqZY  Uai1  sqXAI sqCAI >+> vPlays2' sqZE Gua2  sqZIA
 declare Downward Dat2AIo 
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


fed004' :: Operation ()
fed004' = do
 vPlays2' sqTAI Kauk2 sqTY  >+> vPlays2' sqXI Kauk2 sqXU
 vPlays2' sqXAI Kauk2 sqXY  >+> vPlays2' sqZI Nuak1 sqZU
 vPlays2' sqZAI Nuak1 sqZY  >+> playsTam sqZO     sqXI
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

fed005' :: Operation ()
fed005' = do
 vPlays3' sqTE 虎 sqNI sqTU  >-> vPlays2' sqTAI 兵 sqTY
 vPlays2' sqZI 船 sqZU  >-> vPlays3' sqTAU 虎 sqZAI sqXY
 playsTam sqZO  sqCE  >-> vPlays2' sqXIA 将 sqZAU
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


