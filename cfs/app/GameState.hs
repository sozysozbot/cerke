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
)where
import Board
import PrettyPrint(initialBoard)
import Piece hiding(Piece(..))
import Piece (Piece(),side)
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
plays :: Side -> Square -> Square -> StateT Fullboard M ()
plays sid from to = do
 fb <- get
 case movePieceFromTo from to (board fb) of
  Left (AlreadyOccupied _) -> movePieceFromToTaking sid from to
  Right (side2, newBoard)
   | sid == side2 -> put $ fb{board = newBoard}
   | otherwise -> lift $ Left MovingOpponentPiece
  Left e -> lift $ Left e 

drops :: Side -> (Color, Profession) -> Square -> StateT Fullboard M ()
drops s (c,p) = dropPiece (c,p,s)

passes :: Side -> StateT Fullboard M ()
passes _ = pass

--movePiece_ :: Vec -> Square -> StateT Fullboard Maybe ()
--movePiece_ vec sq = liftBoardOp $ movePiece' vec sq




movePieceFromTo_ :: Square -> Square -> StateT Fullboard M Side
movePieceFromTo_ from to = liftBoardOp $ movePieceFromTo from to


movePieceFromToTaking :: Side -> Square -> Square -> StateT Fullboard M ()
movePieceFromToTaking sid from to = do
 piece <- liftBoardOp $ removePiece to
 if side piece == sid then lift $ Left FriendlyFire else do
  flippedPiece <- lift $ toEither TamCapture $ flipSide piece -- fails for Tam2, which is nice because Tam2 cannot be taken
  actualSide <- movePieceFromTo_ from to
  if actualSide /= sid then lift $ Left MovingOpponentPiece else
   modify (\fb -> fb{hand = flippedPiece : hand fb})

dropPiece :: PhantomPiece -> Square -> StateT Fullboard M ()
dropPiece pp sq = do
 fb <- get
 let pieces = hand fb
 case filter (match pp) pieces of
  [] -> lift $ Left NoCorrespondingPiece -- cannot drop
  (x:xs) -> do
   liftBoardOpFoo $ putPiece x sq -- modify the board,
   modify (\k -> k{hand = xs ++ filter (not . match pp) pieces}) -- modify the hand

pass :: StateT Fullboard M ()
pass = return ()


