module CerkeFS.Internal.Board
(Col(..)
,Row(..)
,Square(..)
,Board1
,Vec(..)
,putPiece
,removePiece
--,movePiece
--,movePieceFromTo
,movePieceFromToFull
,Error(..)
,M
,add
--,toEither
) where
import CerkeFS.Piece3
import qualified Data.Map as M

data Col = CK | CL | CN | CT | CZ | CX | CC | CM | CP deriving(Show, Eq, Ord, Enum)
data Row = RA | RE | RI | RU | RO | RY | RAI | RAU | RIA deriving(Show,Eq, Ord,Enum)
data Square = Square{row :: Row, col :: Col} deriving(Eq, Ord)

instance Show Square where
 show Square{col=c,row=r} = "sq" ++ tail(show c) ++ tail(show r)

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

data Error 
 = AlreadyOccupied Square -- ^ The square you're moving to is already occupied. 
 | EmptySquare Square -- ^ The square you're moving from is actually empty

-- ~ | OutOfBoard 
 | TamCapture -- ^ The square you're moving to is already occupied by Tam2.
 | NoCorrespondingPieceInHand -- ^ You tried to drop a piece that is not in the hand.
 | MovingOpponentPiece -- ^ You tried to move the opponent's piece.
 | FriendlyFire -- ^ You tried to capture your own piece.
 | AmbiguousColor -- ^ Color of the dropped piece cannot be unambiguously inferred.
 | WrongProfessionSpecified {expected :: Maybe Profession, specified :: Maybe Profession} -- ^ The actual profession differs from the expectation.
 | FalseDeclaration -- ^ Declares a Dat2 whose condition is not satisfied.
  deriving(Show, Eq, Ord) 

{-
********************************************
* Primitive operations on the board 
********************************************
-}

isOccupiedFor :: Square -> Board1 -> Bool
isOccupiedFor = M.member

-- | Puts a piece on a square. Fails with 'AlreadyOccupied' if already occupied.
putPiece :: Piece -> Square -> Board1 -> M Board1
putPiece p sq b = if sq `isOccupiedFor` b then Left (AlreadyOccupied sq) else Right(M.insert sq p b)

-- | Removes a piece. Fails with 'EmptySquare' if the specified square is empty.
removePiece :: Square -> Board1 -> M (Piece, Board1)
removePiece sq b = toEither (EmptySquare sq) $ do
 p <- sq `M.lookup` b
 return (p, M.delete sq b)

-- | Moves a piece on a square according to the vector. Raises: 
--
--   * 'OutOfBoard' if the piece goes out of the board
--   * 'EmptySquare' if the original square is empty
--   * 'AlreadyOccupied' if the resulting square is already occupied
{-
movePiece :: Vec -> Square -> Board1 -> M Board1
movePiece vec sq b = do
 (p, new_b) <- removePiece sq b
 new_sq <- toEither OutOfBoard $ add vec sq
 putPiece p new_sq new_b
-}

-- | Moves a piece from a square to a square. Raises: 
--
--   * 'EmptySquare' if the original square is empty
--   * 'AlreadyOccupied' if the resulting square is already occupied
movePieceFromToFull :: Square -> Square -> Board1 -> M (Maybe PhantomPiece, Board1)
movePieceFromToFull from to b = do
 (p, new_b) <- removePiece from b
 newerBoard <- putPiece p to new_b
 return (toPhantom p, newerBoard)