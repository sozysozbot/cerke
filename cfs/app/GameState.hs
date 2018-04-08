module GameState
(Fullboard(..)
)where
import Board
import Piece
data Fullboard = Fullboard{
 turn :: Side,
 board :: Board1,
 hand :: [Piece] -- whose hand the piece is in is designated in Piece itself
}