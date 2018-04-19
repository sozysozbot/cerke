module CerkeFS.VerifyRange
(vPlays2
,vPlays3
,vPlays3'
,vPlays2'
,vPlTam2
,vPlTam3
) where
import CerkeFS.Internal.Board
import CerkeFS.Board2
import CerkeFS.Piece3
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import CerkeFS.GameState
import CerkeFS.VerifyDisplacement

import Control.Exception.Base(assert)
import Control.Monad(unless, when)
import Data.List(intersect)

-- | Plays Tam2, which did not step on a piece.
--
-- Thus, Tam2 must have visited an empty square, then to the destination.
vPlTam2 :: Square -> Square -> Side -> Operation ()
vPlTam2 from to sid = do 
 playsTam from to sid
 Fullboard{board = b} <- get
 let candidate = getNeighbors from `intersect` getNeighbors to
 when (all (`isOccupied` b) candidate) $
  lift $ Left (Tam2PrivilegeExceeded from Nothing to)

-- | __/FIXME/__
vPlTam3 :: Square -> Square -> Square -> Side -> Operation ()
vPlTam3 from thru to sid = do
 verifyNonEmpty thru
 playsTam from to sid
 return ()

-- | Similar to 'plays', but checks whether the move is allowed by the profession.
vPlays2 :: Square -> Square -> Side -> Operation ()
vPlays2 from to sid = do
 phantom <- plays from to sid
 case phantom of
  Nothing -> error "Tam2 not handled"
  Just(_,p,s) -> do
   assert(s == sid) $ return ()
   verifyDisplacement s p from to


-- | Similar to 'plays', but checks whether the move is allowed by the profession.
--
-- The first arguments denotes the origin, the second denotes the square to step on, and the third denotes the destination.
vPlays3 :: Square -> Square -> Square -> Side -> Operation ()
vPlays3 from thru to sid = do
 verifyNonEmpty thru
 phantom <- plays from to sid
 case phantom of
  Nothing -> error "Tam2 not handled"
  Just(_,p,s) -> do
   assert(s == sid) $ return ()
   verifyDisplacement s p from thru
   verifyDisplacement s p thru to

verifyNonEmpty :: Square -> Operation ()
verifyNonEmpty thru = do
 Fullboard{board = b} <- get
 unless(thru `isOccupied` b) $
  lift $ Left (SteppingEmptySquare thru)


-- | Similar to 'plays'', but checks whether the move is allowed by the profession.
vPlays2' :: Square -> Profession -> Square -> Side -> Operation ()
vPlays2' from prof to sid = do
 Just(_,p,s) <- plays' from prof to sid
 assert(p == prof && s == sid) $ return () -- well, we already checked
 verifyDisplacement s p from to

-- | Similar to 'plays'', but checks whether the move is allowed by the profession.
--
-- The first arguments denotes the origin, the third denotes the square to step on, and the fourth denotes the destination.
vPlays3' :: Square -> Profession -> Square -> Square -> Side -> Operation ()
vPlays3' from prof thru to sid = do
 verifyNonEmpty thru
 Just(_,p,s) <- plays' from prof to sid
 assert(p == prof && s == sid) $ return () -- well, we already checked
 verifyDisplacement s p from thru
 verifyDisplacement s p thru to
