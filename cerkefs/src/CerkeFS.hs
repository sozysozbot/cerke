{-|
Module      : CerkeFS
Description : Public APIs for the CerkeFS. It should be possible to depict most valid operations with functions exported from this module.
Maintainer  : sozysozbot@gmail.com
Stability   : experimental

CerkeFS, which stands for cerke'd fedirrgavir'i slergyl, is a library to check whether the given record of the game of Cerke is valid or not. It should be possible to depict most valid operations with functions exported from this module.
-}
module CerkeFS
(module CerkeFS.Board2
,module CerkeFS.PrettyPrint
,module Control.Monad.Trans.Class
,module Control.Monad.Trans.State.Lazy
,module CerkeFS.Piece3
,vPlays2
,vPlays3
,vPlays3'
,vPlays2'
,vPlTam2
,vPlTam3
,Operation
,taxot1
,drops
,drops'
,mun1
,declare
,(>+>),(>->)
,Dat2(..)
,Fullboard(..)
,initialBoard
,initialFullBoard
,playFromStart
,toDebugOutput
,Move(..)
,testAll
,testAll'
) where
import CerkeFS.Board2
--import CerkeFS.GameState
import CerkeFS.PrettyPrint
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import CerkeFS.Piece3
import CerkeFS.VerifyRange
import CerkeFS.InitialBoard

initialFullBoard :: Fullboard
initialFullBoard = Fullboard{board = initialBoard, hand = []}

-- | Applies the given operation to 'initialBoard'.
playFromStart :: Monad m => StateT Fullboard m a -> m Fullboard
playFromStart p = execStateT p initialFullBoard

-- | Similar to 'playFromStart', but outputs in the concise ASCII format.
--
-- >>> toDebugOutput $ do{vPlays3' sqKE  Tuk2 sqLE sqNE Downward; vPlays2' sqTAI Kauk2 sqTY Upward}
-- "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n - _2k_7k_4k - _4h - _2h_7h\n_1h_1k_1h_1k_!k_1k_1h_1k_1h\n -  -  -  -  -  -  -  -  - \n -  -  -  - ^$h -  -  -  - \n -  -  - ^1k -  -  -  -  - \n^1h^1k^1h - ^!h^1k^1h^1k^1h\n^7h^2h - ^4h - ^4k - ^2k^7k\n^6k^5k^3k^8k^#h^8h^3h^5h^6h\n~~~\n\n"
toDebugOutput :: Operation a2 -> String
toDebugOutput fed = case playFromStart fed of
  Left e -> "error: " ++ show e
  Right Fullboard{board = final, hand = pieces} -> 
   drawBoard final ++ "~~~\n" ++ concatMap (convertPieceToStr . toFullPiece) pieces ++ "\n"

