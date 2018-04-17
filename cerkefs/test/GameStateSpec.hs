module GameStateSpec (spec) where

import Test.Hspec
import CerkeFS

spec :: Spec
spec = do
  describe "error detections" $ do
    it "Moving opponent's piece" $ 
      Left MovingOpponentPiece `shouldBeGivenBy` plays sqTAU sqTY Downward
    it "Cannot capture your own piece" $ 
      Left FriendlyFire `shouldBeGivenBy` plays sqKAU sqLAU Upward
    it "Cannot move from an empty square" $ 
      Left(EmptySquare sqNAU) `shouldBeGivenBy` plays sqNAU sqLAU Upward
    it "Dropping when there is no corresponding piece in hand" $
      Left NoCorrespondingPieceInHand `shouldBeGivenBy` drops (Kok1, Maun1) sqKY Upward
    it "Capturing Tam2" $
      Left TamCapture `shouldBeGivenBy` do
        plays sqZO sqCE >+> plays sqXA sqCE
    it "Ambiguous color" $
      Left AmbiguousColor `shouldBeGivenBy` do
        plays sqTAI sqTY >+> plays sqTI sqTU
        plays sqTY  sqTU >+> plays sqNI sqNO
        plays sqNAI sqNO >+> plays sqZA sqNE
        drops' Kauk2 sqNAU Upward
    it "Illegal move is illegal even if wrapped by mun1" $
      Left MovingOpponentPiece `shouldBeGivenBy` mun1 (plays sqTAU sqTY) Downward
    it "Wrong profession specified" $
      Left WrongProfessionSpecified {expected = Just Dau2, specified = Just Kauk2} `shouldBeGivenBy` do
        plays' sqTAU 兵 sqTY Upward 
    it "False declaration of Dat2" $
      Left FalseDeclaration `shouldBeGivenBy` declare Downward Saup1
    it "Dropping to an occupied square" $
      Left (AlreadyOccupied sqKE) `shouldBeGivenBy` do 
        plays sqTAI sqTY >+> plays sqTI sqTU
        plays sqTY  sqTU >+> plays sqNI sqNO
        drops' Kauk2 sqKE Upward
    it "Tam2HueAUai1 by position" $
      Left Tam2HueAUai1Violation `shouldBeGivenBy` do
        plays sqNAI sqNY  >+> plays sqLA sqXO
        plays sqTIA sqNAI >+> plays sqXO sqTAI
    it "Tam2HueAUai1 by Tam2" $
      Left Tam2HueAUai1Violation `shouldBeGivenBy` do
        plays sqZAI sqZY  >+> plays sqZA sqCE
        plays sqTIA sqZAI >+> plays sqKE sqNE
        plays sqZO  sqZAU >+> plays sqZI sqZY

shouldBeGivenBy b a = playFromStart a `shouldBe` b
