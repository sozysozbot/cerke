module CerkeFS.Internal.BoardSpec (spec) where

import Test.Hspec
import CerkeFS
import CerkeFS.Internal.Board
import CerkeFS.Operations
import CerkeFS.TestUtilities
import Data.Maybe

spec :: Spec
spec = do
  describe "getNeighborsAndSelf" $ mapM_ u [0..80]
  describe "isInherentTam2Hue'" $ mapM_ v [0..80]


u i = do
    it ("getNeighborsAndSelf "++show i) $ 
      getNeighborsAndSelf i `shouldBeSimilarTo` getNeighborsAndSelf_old i 

v i = do
    it ("isInherentTam2Hue' "++show i) $
     isInherentTam2Hue' i `shouldBe` (i `elem` map fromSquare [sqNI, sqNAI, sqTU, sqTY, sqZO, sqXU, sqXY, sqCI, sqCAI])


getNeighborsAndSelf_old sq = mapMaybe (`add_` sq) [Vec a b | a <- [-1,0,1], b <- [-1,0,1]]