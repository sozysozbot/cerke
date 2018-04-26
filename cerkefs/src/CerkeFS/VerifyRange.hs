module CerkeFS.VerifyRange
(vPlays2
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
,testAll
,Move(..)
) where
import CerkeFS.Internal.Board
import CerkeFS.Piece3
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import CerkeFS.Operations
import CerkeFS.VerifyDisplacement

import Control.Exception.Base(assert)
import Control.Monad(unless)
import Data.List(intersect, partition)
import Data.Either(isRight)

data Move = Drop (Color, Profession) Square | Move2 Square Square | Move3 Square Square Square deriving(Show, Eq, Ord)

-- | Generates all the possible moves. Implemented here to gain the speed performance.
testAll :: Side -> Fullboard -> [Move]
testAll sid fb =
 [ Move2 from to | from <- m, to <- l, isValid2 sid fb from to, from /= to ] ++
 [ Move3 from thru to | from <- m, thru <- nes, to <- l, isValid3 sid fb from thru to, from /= to] ++
 [ Drop (c,p) to | (c,p,s) <- map toPhantom' (hand fb), s == sid, to <- es ]
  where 
   l = sqList
   m = ofSide sid fb
   (nes,es) = nonEmpty_empty fb
isValid2 :: Side -> Fullboard -> Square -> Square -> Bool
isValid2 sid fb from to = isRight $ execStateT (vPlays2 from to sid) fb

isValid3 :: Side -> Fullboard -> Square -> Square -> Square -> Bool
isValid3 sid fb from thru to = isRight $ execStateT (vPlays3 from thru to sid) fb

nonEmpty_empty :: Fullboard -> ([Square], [Square])
nonEmpty_empty Fullboard{board = b} = partition (`isOccupied` b) sqList
{-
nonEmptySquares :: Fullboard -> [Square]
nonEmptySquares Fullboard{board = b} = [ sq | sq <- sqList, sq `isOccupied` b]

emptySquares :: Fullboard -> [Square]
emptySquares Fullboard{board = b} = [ sq | sq <- sqList, not(sq `isOccupied` b)]
-}
ofSide :: Side -> Fullboard -> [Square]
ofSide sid Fullboard{board = b} = do
 sq <- sqList
 Just piece <- [sq `lookup_` b]
 case toPhantom piece of
  Nothing {- phantomTam -} -> return sq
  Just (_,_,s) -> if s == sid then return sq else []



-- | Plays Tam2, which did not step on a piece.
--
-- Thus, Tam2 must have visited an empty square, then to the destination.
vPlTam2 :: Square -> Square -> Side -> Operation ()
vPlTam2 from to sid = do 
 playsTam from to sid
 vPlTam2_latter from to

vPlTam2_latter :: Square -> Square -> Operation ()
vPlTam2_latter from to = do
 Fullboard{board = b} <- get
 unless(doesMeetingPlaceExist b from to) $
  lift $ Left (Tam2PrivilegeExceeded from Nothing to)

-- | Checks whether there exists an unoccupied square X such that it is the neighbor of both alpha and beta.
--
-- If candidate is empty, returns False as expected.
doesMeetingPlaceExist :: Board1 -> Square -> Square -> Bool
doesMeetingPlaceExist b alpha beta = not(all (`isOccupied'` b) candidate)
 where candidate = getNeighbors alpha `intersect` getNeighbors beta  

-- | Plays Tam2, which stepped on a piece.
vPlTam3 :: Square -> Square -> Square -> Side -> Operation ()
vPlTam3 from thru to sid = do
 verifyNonEmpty thru
 playsTam from to sid
 vPlTam3_latter from thru to

vPlTam3_latter :: Square -> Square -> Square -> Operation ()
vPlTam3_latter from thru to = do
 Fullboard{board = b} <- get
 if 
   (from `isNeighborOf` thru && doesMeetingPlaceExist b thru to) || 
    -- [from ~> thru ~> somewhere] + [somewhere ~> to]
   (thru `isNeighborOf` to && doesMeetingPlaceExist b from thru)
    -- [from ~> somewhere] + [somewhere ~> thru ~> to]
  then return ()
  else lift $ Left (Tam2PrivilegeExceeded from (Just thru) to)
 



-- | Similar to 'plays', but checks whether the move is allowed by the profession. Tam2 is allowed.
vPlays2 :: Square -> Square -> Side -> Operation ()
vPlays2 from to sid = do
 phantom <- plays from to sid
 case phantom of
  Nothing -> vPlTam2_latter from to
  Just(_,p,s) -> do
   assert(s == sid) $ return ()
   verifyDisplacement s p from to


-- | Similar to 'plays', but checks whether the move is allowed by the profession. Tam2 is allowed.
--
-- The first arguments denotes the origin, the second denotes the square to step on, and the third denotes the destination.
vPlays3 :: Square -> Square -> Square -> Side -> Operation ()
vPlays3 from thru to sid = do
 verifyNonEmpty thru
 phantom <- plays from to sid
 case phantom of
  Nothing -> vPlTam3_latter from thru to
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
