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


liftBoardOp :: StateT Board1 M a -> StateT Fullboard M a
liftBoardOp op = do
 fb <- get
 (a, newBoard) <- lift $ runStateT op (board fb)
 put $ fb{board = newBoard}
 return a


{-
**********
* Monadic board operation
**********
-}
putPiece' :: Piece -> Square -> StateT Board1 M ()
putPiece' p sq = StateT (foo . putPiece p sq )

removePiece' :: Square -> StateT Board1 M Piece
removePiece' sq = StateT (removePiece sq)

--movePiece' :: Vec -> Square -> StateT Board1 Maybe ()
--movePiece' vec sq = StateT (foo . movePiece vec sq )

movePieceFromTo' :: Square -> Square -> StateT Board1 M ()
movePieceFromTo' from to = StateT (foo . movePieceFromTo from to )

{-
**********
* Monadic fullboard operation
**********
-}

--movePiece_ :: Vec -> Square -> StateT Fullboard Maybe ()
--movePiece_ vec sq = liftBoardOp $ movePiece' vec sq

movePieceFromTo_ :: Square -> Square -> StateT Fullboard M ()
movePieceFromTo_ from to = liftBoardOp $ movePieceFromTo' from to

movePieceFromToTaking :: Square -> Square -> StateT Fullboard M ()
movePieceFromToTaking from to = do
 piece <- liftBoardOp $ removePiece' to
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
   liftBoardOp $ putPiece' x sq -- modify the board,
   modify (\k -> k{hand = xs ++ filter (not . match pp) pieces}) -- modify the hand

{-
**********
* Example
**********
-}




foo :: M b -> M((), b)
foo k = do
 u <- k
 return((),u)
