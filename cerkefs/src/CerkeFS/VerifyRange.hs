module CerkeFS.VerifyRange
(vPlays2
,vPlays3
,vPlays3'
,vPlays2'
) where
import CerkeFS.Internal.Board
import CerkeFS.Piece3
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.MultiSet as S
import CerkeFS.GameState

-- | Similar to 'plays', but checks whether the move is allowed by the profession.
--
-- __/FIXME: Verification not implemented/__
vPlays2 :: Square -> Square -> Side -> Operation ()
vPlays2 from to sid = plays from to sid

-- | Similar to 'plays', but checks whether the move is allowed by the profession.
--
-- | The first arguments denotes the origin, the second denotes the square to step on, and the third denotes the destination.
--
-- __/FIXME: Verification not implemented/__
vPlays3 :: Square -> Square -> Square -> Side -> Operation ()
vPlays3 from thru to sid = plays from to sid

-- | Similar to 'plays'', but checks whether the move is allowed by the profession.
--
-- __/FIXME: Verification not implemented/__
vPlays2' :: Square -> Profession -> Square -> Side -> Operation ()
vPlays2' from prof to sid = plays' from prof to sid

-- | Similar to 'plays'', but checks whether the move is allowed by the profession.
--
-- | The first arguments denotes the origin, the third denotes the square to step on, and the fourth denotes the destination.
--
-- __/FIXME: Verification not implemented/__
vPlays3' :: Square -> Profession -> Square -> Square -> Side -> Operation ()
vPlays3' from prof thru to sid = plays' from prof to sid
