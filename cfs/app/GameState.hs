module GameState
(Fullboard(..)
,play
)where
import Board
import Piece
import Control.Monad.Trans.State.Lazy

data Fullboard = Fullboard{
 turn :: Side,
 board :: Board1,
 hand :: [Piece] -- whose hand the piece is in is designated in Piece itself
}

{-
**********
* Monadic
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

play :: Board1 -> Maybe Board1
play = execStateT $ do
 movePieceFromTo' (Square RAU CT) (Square RY CT)
 movePieceFromTo' (Square RI CN)  (Square RU CN)
 movePieceFromTo' (Square RA CM)  (Square RO CT)
 horse <- removePiece'     (Square RO CT)
 movePieceFromTo' (Square RY CT)  (Square RO CT)
 movePieceFromTo' (Square RA CT)  (Square RI CN)
 movePieceFromTo' (Square RAU CL)  (Square RO CL)
 movePieceFromTo' (Square RA CZ)  (Square RE CZ)
 putPiece' horse{side=Upward} (Square RY CK)
 king <- removePiece' (Square RE CZ)
 movePieceFromTo' (Square RY CK) (Square RE CZ) 
 horse2 <- removePiece' (Square RE CZ) 
 movePieceFromTo' (Square RA CX)  (Square RE CZ)



foo :: Maybe b -> Maybe((), b)
foo k = do
 u <- k
 return((),u)
