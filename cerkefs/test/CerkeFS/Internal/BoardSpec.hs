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

u i = do
    it ("getNeighborsAndSelf "++show i) $ 
      getNeighborsAndSelf i `shouldBeSimilarTo` getNeighborsAndSelf_old i 

getNeighborsAndSelf_old sq = mapMaybe (`add_` sq) [Vec a b | a <- [-1,0,1], b <- [-1,0,1]]