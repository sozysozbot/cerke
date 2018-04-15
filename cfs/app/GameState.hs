module GameState
(Fullboard(..)
,movePieceFromTo_
,movePieceFromToTaking
,dropPiece
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
,playsT
)where
import Board
import PrettyPrint(initialBoard)
import Piece3
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
-- implicitly takes the piece, if blocked
-- FIXME: think about the side
plays :: Square -> Square -> Side -> StateT Fullboard M ()
plays from to sid = do
 fb <- get
 case movePieceFromTo from to (board fb) of
  Left (AlreadyOccupied _) -> movePieceFromToTaking sid from to
  Right (side2, newBoard)
   | hasPrivilege sid side2 -> put $ fb{board = newBoard}
   | otherwise -> lift $ Left MovingOpponentPiece
  Left e -> lift $ Left e 

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
passes _ = pass

--movePiece_ :: Vec -> Square -> StateT Fullboard Maybe ()
--movePiece_ vec sq = liftBoardOp $ movePiece' vec sq



hasPrivilege :: Side -> Maybe Side -> Bool
hasPrivilege _ Nothing = True -- tam2 can be moved by either player
hasPrivilege sid (Just k) = sid == k

movePieceFromTo_ :: Square -> Square -> StateT Fullboard M (Maybe Side)
movePieceFromTo_ from to = liftBoardOp $ movePieceFromTo from to


movePieceFromToTaking :: Side -> Square -> Square -> StateT Fullboard M ()
movePieceFromToTaking sid from to = do
 piece <- liftBoardOp $ removePiece to
 case getSide piece of
  Nothing -> lift $ Left TamCapture
  Just s -> if s == sid then lift $ Left FriendlyFire else do
   let Just flippedPiece = flipSide piece -- fails for Tam2, which doesn't belong here
   actualSide <- movePieceFromTo_ from to
   if hasPrivilege sid actualSide
    then modify (\fb -> fb{hand = flippedPiece : hand fb})
    else lift $ Left MovingOpponentPiece

dropPiece :: PhantomPiece -> Square -> StateT Fullboard M ()
dropPiece pp sq = do
 fb <- get
 let pieces = hand fb
 case filter (match pp) pieces of
  [] -> lift $ Left NoCorrespondingPieceInHand -- cannot drop
  (x:xs) -> do
   liftBoardOpFoo $ putPiece x sq -- modify the board,
   modify (\k -> k{hand = xs ++ filter (not . match pp) pieces}) -- modify the hand

pass :: StateT Fullboard M ()
pass = return ()


mun1 :: (Side -> StateT Fullboard M ()) -> Side -> StateT Fullboard M ()
mun1 action side = do
 fb <- get
 case action side `runStateT` fb of
  Left e -> lift $ Left e
  Right _ -> passes side -- discard the result and allow


plays' :: Square -> Profession -> Square -> Side -> StateT Fullboard M ()
plays' a b c = plays a c

-- plays Tam2
playsT :: Square -> Square -> Side -> StateT Fullboard M ()
playsT = plays 


