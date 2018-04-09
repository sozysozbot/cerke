module Board
(Col(..)
,Row(..)
,Square(..)
,Board1
,Vec(..)
,putPiece
,removePiece
,movePiece
,movePieceFromTo
,Error(..)
,M
,toEither
) where
import Piece (Piece())
import qualified Data.Map as M

data Col = CK | CL | CN | CT | CZ | CX | CC | CM | CP deriving(Show, Eq, Ord, Enum)
data Row = RA | RE | RI | RU | RO | RY | RAI | RAU | RIA deriving(Show,Eq, Ord,Enum)
data Square = Square{row :: Row, col :: Col} deriving(Eq, Ord,Show)
{-
instance Show Square where
 show (Square{col=c,row=r}) = tail(show c) ++ tail(show r)
-}
type Board1 = M.Map Square Piece

type M = Either Error

data Vec = Vec{dx :: Int, dy :: Int}

add :: Vec -> Square -> Maybe Square
add (Vec x y) Square{col=c,row=r} = do
 new_c <- add' x c
 new_r <- add' y r
 return Square{col=new_c, row=new_r}

add' :: (Enum a) => Int -> a -> Maybe a
add' a c
 | a+b < 0 = Nothing  
 | a+b > 8 = Nothing  
 | otherwise = Just $ toEnum (a+b)
 where b = fromEnum c


toEither :: c -> Maybe a -> Either c a
toEither = (`maybe` Right) . Left

data Error = AlreadyOccupied Square | EmptySquare Square | OutOfBoard | TamCapture | NoCorrespondingPiece
 deriving(Show, Eq, Ord)

{-
********************************************
* Primitive operations on the board 
********************************************
-}

isOccupiedFor :: Square -> Board1 -> Bool
isOccupiedFor = M.member

-- put a piece on a square. fails if already occupied.
putPiece :: Piece -> Square -> Board1 -> M Board1
putPiece p sq b = if sq `isOccupiedFor` b then Left (AlreadyOccupied sq) else Right(M.insert sq p b)

-- remove a piece. fails if empty.
removePiece :: Square -> Board1 -> M (Piece, Board1)
removePiece sq b = toEither (EmptySquare sq) $ do
 p <- sq `M.lookup` b
 return (p, M.delete sq b)

-- move a piece on a square according to the vector. fails if:
--  the piece goes out of the board
--  the original square is empty
--  the resulting square is already occupied
movePiece :: Vec -> Square -> Board1 -> M Board1
movePiece vec sq b = do
 (p, new_b) <- removePiece sq b
 new_sq <- toEither OutOfBoard $ add vec sq
 putPiece p new_sq new_b

movePieceFromTo :: Square -> Square -> Board1 -> M Board1
movePieceFromTo from to b = do
 (p, new_b) <- removePiece from b
 putPiece p to new_b

