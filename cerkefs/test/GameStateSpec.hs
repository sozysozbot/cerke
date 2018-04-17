module GameStateSpec (spec) where

import Test.Hspec
import CerkeFS

spec :: Spec
spec = do
  describe "error detections" $ do
    it "error detection 000" $ 
      Left MovingOpponentPiece `shouldBeGivenBy` plays sqTAU sqTY Downward
    it "error detection 001" $ 
      Left FriendlyFire `shouldBeGivenBy` plays sqKAU sqLAU Upward
    it "error detection 002" $ 
      Left(EmptySquare sqNAU) `shouldBeGivenBy` plays sqNAU sqLAU Upward
    it "error detection 003" $
      Left NoCorrespondingPieceInHand `shouldBeGivenBy` drops (Kok1, Maun1) sqKY Upward
    it "error detection 004" $
      Left TamCapture `shouldBeGivenBy` do
        plays sqZO sqCE >+> plays sqXA sqCE
    it "error detection 005" $
      Left AmbiguousColor `shouldBeGivenBy` do
        plays sqTAI sqTY >+> plays sqTI sqTU
        plays sqTY  sqTU >+> plays sqNI sqNO
        plays sqNAI sqNO >+> plays sqZA sqNE
        drops' Kauk2 sqNAU Upward
    it "error detection 006" $
      Left MovingOpponentPiece `shouldBeGivenBy` mun1 (plays sqTAU sqTY) Downward
    it "error detection 007" $
      Left WrongProfessionSpecified {expected = Just Dau2, specified = Just Kauk2} `shouldBeGivenBy` do
        plays' sqTAU 兵 sqTY Upward 
    it "error detection 008" $
      Left FalseDeclaration `shouldBeGivenBy` declare Downward Saup1
    it "error detection 009" $
      Left (AlreadyOccupied sqKE) `shouldBeGivenBy` do 
        plays sqTAI sqTY >+> plays sqTI sqTU
        plays sqTY  sqTU >+> plays sqNI sqNO
        drops' Kauk2 sqKE Upward


shouldBeGivenBy b a = playFromStart a `shouldBe` b
