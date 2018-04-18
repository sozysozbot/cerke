module CerkeFS
(module CerkeFS.Board2
,module CerkeFS.GameState
,module CerkeFS.PrettyPrint
,module Control.Monad.Trans.State.Lazy
,module CerkeFS.Piece3
,module CerkeFS.VerifyRange
,module CerkeFS.InitialBoard
,playFromStart
,toDebugOutput
) where
import CerkeFS.Board2
import CerkeFS.GameState
import CerkeFS.PrettyPrint
import Control.Monad.Trans.State.Lazy
import CerkeFS.Piece3
import CerkeFS.VerifyRange
import CerkeFS.InitialBoard

-- | Applies the given operation to 'initialBoard'.
playFromStart :: Monad m => StateT Fullboard m a -> m Fullboard
playFromStart p = execStateT p Fullboard{board = initialBoard, hand = []}

-- | Similar to 'playFromStart', but outputs in the concise ASCII format.
--
-- >>> toDebugOutput $ do{vPlays3' sqKE  Tuk2 sqLE sqNE Downward; vPlays2' sqTAI Kauk2 sqTY Upward}
-- "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n - _2k_7k_4k - _4h - _2h_7h\n_1h_1k_1h_1k_!k_1k_1h_1k_1h\n -  -  -  -  -  -  -  -  - \n -  -  -  - ^$h -  -  -  - \n -  -  - ^1k -  -  -  -  - \n^1h^1k^1h - ^!h^1k^1h^1k^1h\n^7h^2h - ^4h - ^4k - ^2k^7k\n^6k^5k^3k^8k^#h^8h^3h^5h^6h\n~~~\n\n"
toDebugOutput :: Operation a2 -> String
toDebugOutput fed = case playFromStart fed of
  Left e -> "error: " ++ show e
  Right Fullboard{board = final, hand = pieces} -> 
   drawBoard final ++ "~~~\n" ++ concatMap convertPieceToStr pieces ++ "\n"

