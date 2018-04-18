module CerkeFS.GameStateSpec (spec) where

import Test.Hspec
import CerkeFS

shouldBeGivenBy b a = playFromStart a `shouldBe` b
shouldBeThrownBy b a = playFromStart a `shouldBe` Left b

shouldBeGeneratedBy b a = toDebugOutput a `shouldBe` b

spec :: Spec
spec = do
  describe "error detections" $ do
    it "Moving opponent's piece" $ 
      MovingOpponentPiece `shouldBeThrownBy` plays sqTAU sqTY Downward
    it "Cannot capture your own piece" $ 
      FriendlyFire `shouldBeThrownBy` plays sqKAU sqLAU Upward
    it "Cannot move from an empty square" $ 
      EmptySquare sqNAU `shouldBeThrownBy` plays sqNAU sqLAU Upward
    it "Dropping when there is no corresponding piece in hand" $
      NoCorrespondingPieceInHand `shouldBeThrownBy` drops (Kok1, Maun1) sqKY Upward
    it "Capturing Tam2" $
      TamCapture `shouldBeThrownBy` do
        plays sqZO sqCE >+> plays sqXA sqCE
    it "Ambiguous color" $
      AmbiguousColor `shouldBeThrownBy` do
        plays sqTAI sqTY >+> plays sqTI sqTU
        plays sqTY  sqTU >+> plays sqNI sqNO
        plays sqNAI sqNO >+> plays sqZA sqNE
        drops' Kauk2 sqNAU Upward
    it "Illegal move is illegal even if wrapped by mun1" $
      MovingOpponentPiece `shouldBeThrownBy` mun1 (plays sqTAU sqTY) Downward
    it "Wrong profession specified" $
      WrongProfessionSpecified {expected = Just Dau2, specified = Just Kauk2} `shouldBeThrownBy` do
        plays' sqTAU 兵 sqTY Upward 
    it "False declaration of Dat2" $
      FalseDeclaration `shouldBeThrownBy` declare Downward Saup1
    it "Dropping to an occupied square" $
      AlreadyOccupied sqKE `shouldBeThrownBy` do 
        plays sqTAI sqTY >+> plays sqTI sqTU
        plays sqTY  sqTU >+> plays sqNI sqNO
        drops' Kauk2 sqKE Upward
    it "Tam2HueAUai1 by position" $
      Tam2HueAUai1Violation `shouldBeThrownBy` do
        plays sqNAI sqNY  >+> plays sqLA sqXO
        plays sqTIA sqNAI >+> plays sqXO sqTAI
    it "Tam2HueAUai1 by Tam2" $
      Tam2HueAUai1Violation `shouldBeThrownBy` do
        plays sqZAI sqZY  >+> plays sqZA sqCE
        plays sqTIA sqZAI >+> plays sqKE sqNE
        plays sqZO  sqZAU >+> plays sqZI sqZY
    it "privilege exceeded by Kauk2" $
      ProfessionPrivilegeExceeded Kauk2 sqTI `shouldBeThrownBy` vPlays2 sqTI sqNE Downward
    it "privilege exceeded by Kauk2" $
      ProfessionPrivilegeExceeded Kauk2 sqTI `shouldBeThrownBy` vPlays2 sqTI sqTO Downward
    it "privilege exceeded by Kauk2" $
      ProfessionPrivilegeExceeded Kauk2 sqKAI `shouldBeThrownBy` vPlays2 sqKAI sqZAU Upward
    it "privilege exceeded by Kauk2, but MovingOpponentPiece takes precedence" $
      MovingOpponentPiece `shouldBeThrownBy` vPlays2 sqKAI sqZAU Downward
    it "fedirrgavir 000 with validator" $ 
      ProfessionPrivilegeExceeded Dau2 sqTY `shouldBeThrownBy` fed000'
  describe "correct behaviors" $ do
    it "fedirrgavir 000 (incorrect fedirrgavir, but passes the check because it uses plays' not vPlays3')" $ 
     res000 `shouldBeGeneratedBy` fed000
    it "fedirrgavir 001" $ res001 `shouldBeGeneratedBy` fed001
    it "fedirrgavir 002" $ res002 `shouldBeGeneratedBy` fed002
    it "fedirrgavir 003" $ res003 `shouldBeGeneratedBy` fed003
    it "fedirrgavir 004" $ res004 `shouldBeGeneratedBy` fed004
    it "fedirrgavir 005" $ res005 `shouldBeGeneratedBy` fed005
    it "fedirrgavir 001 with validator" $ res001 `shouldBeGeneratedBy` fed001'
    it "fedirrgavir 002 with validator" $ res002 `shouldBeGeneratedBy` fed002'
    it "fedirrgavir 003 with validator" $ res003 `shouldBeGeneratedBy` fed003'
    it "fedirrgavir 004 with validator" $ res004 `shouldBeGeneratedBy` fed004'
    it "fedirrgavir 005 with validator" $ res005 `shouldBeGeneratedBy` fed005'

fed000 :: Operation ()
fed000 = do
 plays'      sqTAU 虎 sqTY  >+> plays' sqNI 兵 sqNU
 mun1(plays' sqLIA 馬 sqXO) >+> plays' sqMA 馬 sqTO
 plays'      sqTY  虎 sqTO  >+> plays' sqTA 将 sqNI
 plays'      sqLAU 弓 sqLO  >+> plays' sqZA 王 sqZE
 drops'            馬 sqKY  >+> mun1(plays' sqNU 兵 sqNO)
 plays'      sqKY  馬 sqZE  >+> plays' sqXA 将 sqZE
 return ()

fed000' :: Operation ()
fed000' = do
 vPlays3'      sqTAU 虎 sqNAI sqTY  >+> vPlays2' sqNI 兵 sqNU
 mun1(vPlays3' sqLIA 馬 sqTAI sqXO) >+> vPlays3' sqMA 馬 sqXI sqTO
 vPlays2'      sqTY  虎 sqTO  >+> vPlays3' sqTA 将 sqTE sqNI
 vPlays3'      sqLAU 弓 sqLAI sqLO  >+> vPlays2' sqZA 王 sqZE
 drops'            馬 sqKY  >+> mun1(vPlays2' sqNU 兵 sqNO)
 vPlays3'      sqKY  馬 sqNU sqZE  >+> vPlays2' sqXA 将 sqZE
 return ()



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

fed004 :: Operation ()
fed004 = do
 plays' sqTAI Kauk2 sqTY  >+> plays' sqXI Kauk2 sqXU
 plays' sqXAI Kauk2 sqXY  >+> plays' sqZI Nuak1 sqZU
 plays' sqZAI Nuak1 sqZY  >+> playsTam sqZO     sqXI
 plays' sqXAU Dau2  sqZAI >+> plays' sqXA Uai1  sqZI
 plays' sqMAU Gua2  sqZAU >+> plays' sqLE Gua2  sqZE
 plays' sqXIA Uai1  sqXAU >+> plays' sqZA Io    sqNE
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

res000, res001, res002, res003, res004, res005 :: String
res000 = 
 "_6h_5h_3h -  -  - _3k - _6k\n\
 \_7k_2k - _4k_8k_4h - _2h_7h\n\
 \_1h_1k_8h_1k_!k_1k_1h_1k_1h\n\
 \ -  - _1h -  -  -  -  -  - \n\
 \ - ^2h - ^4h^$h -  -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \^1h^1k^1h^1k^!h^1k^1h^1k^1h\n\
 \^7h -  -  -  - ^4k - ^2k^7k\n\
 \^6k^5k^3k^8k^#h^8h^3h^5h^6h\n\
 \~~~\n\
 \_5k^#k\n"
res001 = 
 "_6h_5h - _8h_#k_8k_3k_5k_6k\n\
 \ - _2k -  - _2h -  -  - _7h\n\
 \_1h_1k - _1k -  - _7k_1h_1h\n\
 \ -  - _1h -  - _1k -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  - ^1k - ^1h -  - \n\
 \^1h^1k^1h^4h -  - ^8h^1k^1h\n\
 \^7h^2h -  - ^$h^4k - ^3h^7k\n\
 \^6k - ^3k^8k^#h -  - ^5h^6h\n\
 \~~~\n\
 \_3h_1k_5k^1k^!k_!h^4k_2k_4h\n"
res002 = 
 " -  - _3h - _#k - _3k - _6k\n\
 \ -  -  - _4k_2k -  - _2h - \n\
 \_1h - _7k_1k - _1k_7h_1k_1h\n\
 \_6h - _1h -  - ^1k_1h -  - \n\
 \ - ^8k - ^1k -  -  -  -  - \n\
 \ -  - ^1h -  -  - ^1h -  - \n\
 \^1h -  -  -  - ^8h^!h^1k^1h\n\
 \ - ^2h -  - ^$h^4k^#h^2k^7k\n\
 \^6k^5k^3k -  -  - ^3h^5h^6h\n\
 \~~~\n\
 \^4h_7h^8k^1k_4h^8h^!k_1k^5h^5k\n"
res003 = 
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n\
 \ -  -  - _4k - _4h - _2h_7h\n\
 \_1h_1k_7k -  -  - _1h_1k_1h\n\
 \ -  - _1h_1k - _1k -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  -  -  - ^1h^1k - \n\
 \^1h^1k^1h^1k - ^1k^8h - ^1h\n\
 \^7h^2h - ^4h - ^4k^$h^2k^7k\n\
 \^6k^5k^3k^8k_2k - ^3h^5h^6h\n\
 \~~~\n\
 \_#h^!k_!h\n"
res004 = 
 "_6h_5h_3h -  -  - _3k_5k_6k\n\
 \_7k - _#k_4k_2k_8h -  - _7h\n\
 \_1h_1k_1h_1k_8k^$h_1h_1k_1h\n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  - ^1h^1k^2k - ^1h^1k^7k\n\
 \^1h^1k^4h - ^3h -  -  - ^1h\n\
 \^7h^2h -  -  - ^8h -  -  - \n\
 \^6k^5k^3k^8k_2h -  -  - _4h\n\
 \~~~\n\
 \_#h_5h^!k_!h_6h_4k^1k_1k\n"
res005 = 
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n\
 \_7k -  - _2k - _4h^$h_7h - \n\
 \_1h_1k_1h -  - _1k_1h - _1h\n\
 \ -  -  -  - ^!h -  - _1k - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ - ^2h -  -  -  -  -  -  - \n\
 \^1h^1k^1h_1k - ^1k^1h^1k^1h\n\
 \^7h -  -  - ^8h^4k - ^2k^7k\n\
 \^6k - ^3k^8k - ^#h^3h^5h^6h\n\
 \~~~\n\
 \_5k^!k_4h^2h_1k^4k\n"
