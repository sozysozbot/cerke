module GameState
(Fullboard(..)
,movePieceFromTo_
,movePieceFromToTaking
,dropPiece
)where
import Board
import Piece hiding(Piece(..))
import Piece (Piece())
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

data Fullboard = Fullboard{
-- turn :: Side,
 board :: Board1,
 hand :: [Piece] -- whose hand the piece is in is designated in Piece itself
}


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
 return ()

{-
**********
* Monadic fullboard operation
**********
-}

--movePiece_ :: Vec -> Square -> StateT Fullboard Maybe ()
--movePiece_ vec sq = liftBoardOp $ movePiece' vec sq

movePieceFromTo_ :: Square -> Square -> StateT Fullboard M ()
movePieceFromTo_ from to = liftBoardOpFoo $ movePieceFromTo from to

movePieceFromToTaking :: Square -> Square -> StateT Fullboard M ()
movePieceFromToTaking from to = do
 piece <- liftBoardOp $ removePiece to
 flippedPiece <- lift $ toEither TamCapture $ flipSide piece -- fails for Tam2, which is nice because Tam2 cannot be taken
 movePieceFromTo_ from to
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



