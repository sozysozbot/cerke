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
import CerkeFS.GameState

import Control.Exception.Base(assert)

-- | Similar to 'plays', but checks whether the move is allowed by the profession.
vPlays2 :: Square -> Square -> Side -> Operation ()
vPlays2 from to sid = do
 phantom <- plays from to sid
 case phantom of
  Nothing -> error "Tam2 not handled"
  Just(_,p,s) -> do
   assert(s == sid) $ return ()
   verifyMove s p from to

isTam2Hue' :: Square -> Operation Bool
isTam2Hue' sq = do
 Fullboard{board = b}<- get
 return $ isTam2Hue b sq

verifyMove :: Side -> Profession -> Square -> Square -> Operation ()
verifyMove sid prof from to = do
 let diff = rotate sid $ to `minus` from
 isth <- isTam2Hue' from
 guard' prof from $ case (isth, prof) of
  (False, Kauk2) -> diff `elem` map (uncurry Vec) [(0,1)]
  (False, Dau2 ) -> diff `elem` map (uncurry Vec) [(1,1),(1,-1),(-1,1),(-1,-1)]
  (False, Maun1) -> diff `elem` map (uncurry Vec) [(2,2),(2,-2),(-2,2),(-2,-2)]
  (True , Maun1) -> diff `elem` map (uncurry Vec) [(2,2),(2,0),(2,-2),(0,2),(0,-2),(-2,2),(-2,0),(-2,-2)]
  (False, Tuk2 ) -> diff `elem` map (uncurry Vec) [(0,1),(1,0),(0,-1),(-1,0)]
  (False, Io   ) -> diff `elem` map (uncurry Vec) [(0,1),(1,0),(0,-1),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
  (True , Io   ) -> diff `elem` map (uncurry Vec) [(0,1),(1,0),(0,-1),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
  (True , Uai1 ) -> diff `elem` map (uncurry Vec) [(0,1),(1,0),(0,-1),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
  (False, Uai1 ) -> diff `elem` map (uncurry Vec) [(0,1),(1,0),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
  _ -> True -- FIXME

guard' :: Profession -> Square -> Bool -> Operation ()
guard' prof from False = lift $ Left (ProfessionPrivilegeExceeded prof from)
guard' _ _ _ = return ()

rotate :: Side -> Vec -> Vec
rotate Downward vec = vec
rotate Upward Vec{dx=x, dy=y} = Vec{dx= -x, dy= -y}

-- | Similar to 'plays', but checks whether the move is allowed by the profession.
--
-- | The first arguments denotes the origin, the second denotes the square to step on, and the third denotes the destination.
vPlays3 :: Square -> Square -> Square -> Side -> Operation ()
vPlays3 from thru to sid = do
 verifyNonEmpty thru
 phantom <- plays from to sid
 case phantom of
  Nothing -> error "Tam2 not handled"
  Just(_,p,s) -> do
   assert(s == sid) $ return ()
   verifyMove s p from thru
   verifyMove s p thru to

verifyNonEmpty :: Square -> Operation ()
verifyNonEmpty thru = do
 Fullboard{board = b} <- get
 if not(thru `isOccupied` b)
  then lift $ Left (SteppingEmptySquare thru)
  else return ()


-- | Similar to 'plays'', but checks whether the move is allowed by the profession.
vPlays2' :: Square -> Profession -> Square -> Side -> Operation ()
vPlays2' from prof to sid = do
 Just(_,p,s) <- plays' from prof to sid
 assert(p == prof && s == sid) $ return () -- well, we already checked
 verifyMove s p from to

-- | Similar to 'plays'', but checks whether the move is allowed by the profession.
--
-- | The first arguments denotes the origin, the third denotes the square to step on, and the fourth denotes the destination.
vPlays3' :: Square -> Profession -> Square -> Square -> Side -> Operation ()
vPlays3' from prof thru to sid = do
 verifyNonEmpty thru
 Just(_,p,s) <- plays' from prof to sid
 assert(p == prof && s == sid) $ return () -- well, we already checked
 verifyMove s p from thru
 verifyMove s p thru to
