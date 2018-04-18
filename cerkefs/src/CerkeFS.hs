module CerkeFS
(module CerkeFS.Board2
,module CerkeFS.GameState
,module CerkeFS.PrettyPrint
,module Control.Monad.Trans.State.Lazy
,module CerkeFS.Piece3
,playFromStart
) where
import CerkeFS.Board2
import CerkeFS.GameState
import CerkeFS.PrettyPrint
import Control.Monad.Trans.State.Lazy
import CerkeFS.Piece3
import CerkeFS.InitialBoard

playFromStart :: Monad m => StateT Fullboard m a -> m Fullboard
playFromStart p = execStateT p Fullboard{board = initialBoard, hand = []}
