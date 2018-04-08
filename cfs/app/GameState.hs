module GameState
(Fullboard(..)
,play
,play2
,play3
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


liftBoardOp :: StateT Board1 Maybe a -> StateT Fullboard Maybe a
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
putPiece' :: Piece -> Square -> StateT Board1 Maybe ()
putPiece' p sq = StateT (foo . putPiece p sq )

removePiece' :: Square -> StateT Board1 Maybe Piece
removePiece' sq = StateT (removePiece sq)

movePiece' :: Vec -> Square -> StateT Board1 Maybe ()
movePiece' vec sq = StateT (foo . movePiece vec sq )

movePieceFromTo' :: Square -> Square -> StateT Board1 Maybe ()
movePieceFromTo' from to = StateT (foo . movePieceFromTo from to )

{-
**********
* Monadic fullboard operation
**********
-}
movePieceFromTo_ :: Square -> Square -> StateT Fullboard Maybe ()
movePieceFromTo_ from to = liftBoardOp $ movePieceFromTo' from to

movePieceFromToTaking :: Square -> Square -> StateT Fullboard Maybe ()
movePieceFromToTaking from to = do
 piece <- liftBoardOp $ removePiece' to
 flippedPiece <- lift $ flipSide piece -- fails for Tam2, which is nice because Tam2 cannot be taken
 movePieceFromTo_ from to
 modify (\fb -> fb{hand = flippedPiece : hand fb})



dropPiece :: PhantomPiece -> Square -> StateT Fullboard Maybe ()
dropPiece pp sq = do
 fb <- get
 let pieces = hand fb
 case filter (match pp) pieces of
  [] -> lift $ Nothing -- cannot drop
  (x:xs) -> do
   liftBoardOp $ putPiece' x sq -- modify the board,
   modify (\k -> k{hand = xs}) -- modify the hand

{-
**********
* Example
**********
-}

play3 b = execStateT play2 Fullboard{board = b, hand = []}

play2 :: StateT Fullboard Maybe ()
play2 = do
 movePieceFromTo_ (Square RAU CT) (Square RY CT)
 movePieceFromTo_ (Square RI CN)  (Square RU CN)
 movePieceFromTo_ (Square RA CM)  (Square RO CT)
 movePieceFromToTaking (Square RY CT)  (Square RO CT)
 movePieceFromTo_ (Square RA CT)  (Square RI CN)
 movePieceFromTo_ (Square RAU CL) (Square RO CL)
 movePieceFromTo_ (Square RA CZ)  (Square RE CZ)
 dropPiece (Kok1, Maun1, Upward) (Square RY CK)
 movePieceFromToTaking (Square RY CK) (Square RE CZ) 
 movePieceFromToTaking (Square RA CX)  (Square RE CZ)

play :: Board1 -> Maybe Board1
play = execStateT pl 

pl :: StateT Board1 Maybe ()
pl = do
 movePieceFromTo' (Square RAU CT) (Square RY CT)
 movePieceFromTo' (Square RI CN)  (Square RU CN)
 movePieceFromTo' (Square RA CM)  (Square RO CT)
 horse <- removePiece'     (Square RO CT)
 horse' <- lift $ flipSide horse
 movePieceFromTo' (Square RY CT)  (Square RO CT)
 movePieceFromTo' (Square RA CT)  (Square RI CN)
 movePieceFromTo' (Square RAU CL)  (Square RO CL)
 movePieceFromTo' (Square RA CZ)  (Square RE CZ)
 putPiece' horse' (Square RY CK)
 king <- removePiece' (Square RE CZ)
 movePieceFromTo' (Square RY CK) (Square RE CZ) 
 horse2 <- removePiece' (Square RE CZ) 
 movePieceFromTo' (Square RA CX)  (Square RE CZ)

foo :: Maybe b -> Maybe((), b)
foo k = do
 u <- k
 return((),u)
