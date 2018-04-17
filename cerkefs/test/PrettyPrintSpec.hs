module PrettyPrintSpec (spec) where

import Test.Hspec
import CerkeFS.PrettyPrint
import CerkeFS

initBoardString :: String
initBoardString = 
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n\
 \_7k_2k - _4k - _4h - _2h_7h\n\
 \_1h_1k_1h_1k_!k_1k_1h_1k_1h\n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  - ^$h -  -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \^1h^1k^1h^1k^!h^1k^1h^1k^1h\n\
 \^7h^2h - ^4h - ^4k - ^2k^7k\n\
 \^6k^5k^3k^8k^#h^8h^3h^5h^6h\n" 

spec :: Spec
spec = do
  describe "loadBoard" $ do
   it "initial board" $
    loadBoard initBoardString `shouldBe` Just initialBoard
  describe "drawBoard" $ do
   it "initial board" $
    drawBoard initialBoard `shouldBe` initBoardString
