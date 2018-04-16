module CerkeFS.GameState
(Fullboard(..)
,playFromStart
,initialBoard
--,movePieceFromTo2
--,pass
,plays
,drops
,passes
,drops'
,mun1
,plays'
,playsTam
)where
import CerkeFS.Board
import CerkeFS.PrettyPrint(initialBoard)
import CerkeFS.Piece3
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

data Fullboard = Fullboard{
-- turn :: Maybe Side, -- `Nothing` means no info about the turn
 board :: Board1,
 hand :: [Piece] -- whose hand the piece is in is designated in Piece itself
}


playFromStart :: Monad m => StateT Fullboard m a -> m Fullboard
playFromStart p = execStateT p Fullboard{board = initialBoard, hand = []}

liftBoardOp :: Monad m => (Board1 -> m (b, Board1)) -> StateT Fullboard m b
liftBoardOp op = do
 fb <- get
 (a, newBoard) <- lift $ op (board fb)
 put $ fb{board = newBoard}
 return a

liftBoardOpFoo :: Monad m => (Board1 -> m Board1) -> StateT Fullboard m ()
liftBoardOpFoo op = do
 fb <- get
 newBoard <- lift $ op (board fb)
 put $ fb{board = newBoard}

{-
**********
* Monadic fullboard operation
**********
-}

type Validator = Maybe PhantomPiece -> Either Error ()

-- plays under the condition
validatesPlaying, validatesTaking :: Validator -> 
 (Square, Square, Side) -> StateT Fullboard M ()
validator `validatesPlaying` (from, to, sid) = do
 fb <- get
 case movePieceFromToProf from to (board fb) of
  Left (AlreadyOccupied _) -> validator `validatesTaking` (from, to, sid)
  Right (side_prof, newBoard) -> case validator side_prof of
   Right () -> put $ fb{board = newBoard}
   Left e -> lift $ Left e
  Left e -> lift $ Left e 


-- FIXME: Uai protection
validator `validatesTaking` (from, to, sid) = do
 piece <- liftBoardOp $ removePiece to
 case getSide piece of
  Nothing -> lift $ Left TamCapture
  Just s -> if s == sid then lift $ Left FriendlyFire else do
   let Just flippedPiece = flipSide piece -- fails for Tam2, which doesn't belong here
   side_prof <- liftBoardOp $ movePieceFromToProf from to
   case validator side_prof of
    Right () -> modify (\fb -> fb{hand = flippedPiece : hand fb})
    Left e   -> lift $ Left e

-- implicitly takes the piece, if blocked
plays :: Square -> Square -> Side -> StateT Fullboard M ()
plays from to sid = f `validatesPlaying` (from, to, sid) where
 f Nothing = return ()
 f (Just(_, _, q)) = if sid == q then return () else Left MovingOpponentPiece


plays' :: Square -> Profession -> Square -> Side -> StateT Fullboard M ()
plays' from prof to sid = f `validatesPlaying` (from, to, sid) where
 f Nothing = Left WrongProfessionSpecified{expected = Nothing, specified = Just prof}
 f (Just(_, p, q)) = case(sid == q, prof == p) of
  (False, _) -> Left MovingOpponentPiece -- turn violation is louder than wrong profession
  (True, False) -> Left WrongProfessionSpecified{expected = Just p, specified = Just prof}
  (True, True) -> return ()

playsTam :: Square -> Square -> Side -> StateT Fullboard M ()
playsTam from to sid = f `validatesPlaying` (from, to, sid) where
 f Nothing = return ()
 f (Just(_, p, _))= Left WrongProfessionSpecified{expected = Just p, specified = Nothing}




drops :: (Color, Profession) -> Square -> Side -> StateT Fullboard M ()
drops (c,p) sq s = dropPiece (c,p,s) sq

drops' :: Profession -> Square -> Side -> StateT Fullboard M ()
drops' p sq s = do
 fb <- get
 let kok  = dropPiece (Kok1, p, s) sq `runStateT` fb
 let huok = dropPiece (Huok2, p, s) sq `runStateT` fb
 case (kok, huok) of
  (Right _, Right _) -> lift $ Left AmbiguousColor
  (Left _, Right ((), newBoard)) -> put newBoard
  (Right ((), newBoard), Left _) -> put newBoard
  (Left NoCorrespondingPieceInHand, Left NoCorrespondingPieceInHand) -> lift $ Left NoCorrespondingPieceInHand
  _ -> error "cannot happen hgi8iejrws"


passes :: Side -> StateT Fullboard M ()
passes _ = return ()


dropPiece :: PhantomPiece -> Square -> StateT Fullboard M ()
dropPiece pp sq = do
 fb <- get
 let pieces = hand fb
 case filter (match pp) pieces of
  [] -> lift $ Left NoCorrespondingPieceInHand -- cannot drop
  (x:xs) -> do
   liftBoardOpFoo $ putPiece x sq -- modify the board,
   modify (\k -> k{hand = xs ++ filter (not . match pp) pieces}) -- modify the hand

-- the operation that must theoretically succeed but did not happen
mun1 :: (Side -> StateT Fullboard M ()) -> Side -> StateT Fullboard M ()
mun1 action side = do
 fb <- get
 case action side `runStateT` fb of
  Left e -> lift $ Left e
  Right _ -> passes side -- discard the result and allow


