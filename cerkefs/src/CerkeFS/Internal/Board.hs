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
,add,minus
,getNeighborsAndSelf
,getNeighbors
,sqKA,  sqLA,  sqNA,  sqTA,  sqZA,  sqXA,  sqCA,  sqMA,  sqPA, 
 sqKE,  sqLE,  sqNE,  sqTE,  sqZE,  sqXE,  sqCE,  sqME,  sqPE, 
 sqKI,  sqLI,  sqNI,  sqTI,  sqZI,  sqXI,  sqCI,  sqMI,  sqPI, 
 sqKU,  sqLU,  sqNU,  sqTU,  sqZU,  sqXU,  sqCU,  sqMU,  sqPU, 
 sqKO,  sqLO,  sqNO,  sqTO,  sqZO,  sqXO,  sqCO,  sqMO,  sqPO, 
 sqKY,  sqLY,  sqNY,  sqTY,  sqZY,  sqXY,  sqCY,  sqMY,  sqPY, 
 sqKAI, sqLAI, sqNAI, sqTAI, sqZAI, sqXAI, sqCAI, sqMAI, sqPAI,
 sqKAU, sqLAU, sqNAU, sqTAU, sqZAU, sqXAU, sqCAU, sqMAU, sqPAU,
 sqKIA, sqLIA, sqNIA, sqTIA, sqZIA, sqXIA, sqCIA, sqMIA, sqPIA
,sqList
,isTam2HueAUai1
,isOccupied
,isTam2Hue
,isNeighborOf
--,toEither
) where
import CerkeFS.Piece3
import qualified Data.Map as M
import Control.Monad(guard, unless)
import Data.Maybe(isJust,mapMaybe)

data Col = ColumnK | ColumnL | ColumnN | ColumnT | ColumnZ | ColumnX | ColumnC | ColumnM | ColumnP deriving(Show, Eq, Ord, Enum)
data Row = RowA | RowE | RowI | RowU | RowO | RowY | RowAI | RowAU | RowIA deriving(Show,Eq, Ord,Enum)
data Square = Square{row :: Row, col :: Col} deriving(Eq, Ord)

instance Show Square where
 show Square{col=c,row=r} = "sq" ++ length "Column" `drop` show c ++ length "Row" `drop` show r

type Board1 = M.Map Square Piece


data Vec = Vec{dx :: Int, dy :: Int} deriving(Show, Eq, Ord)

-- | Add a vector to the square to get a new square. 'Nothing' if it goes out of the board.
add :: Vec -> Square -> Maybe Square
add (Vec x y) Square{col=c,row=r} = do
 new_c <- add' x c
 new_r <- add' y r
 return Square{col=new_c, row=new_r}

-- | Takes the difference of two squares as a vec. @p `minus` q@ is p minus q.
minus :: Square -> Square -> Vec
minus Square{col=c1,row=r1} Square{col=c2,row=r2}
 = Vec{dx = fromEnum c1 - fromEnum c2, dy = fromEnum r1 - fromEnum r2}

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
 | Tam2HueAUai1Violation -- ^ Tried to take a piece protected by Tam2HueAUai1
 | SteppingEmptySquare Square -- ^ Tried to step on an empty square
 | ProfessionPrivilegeExceeded Profession Square -- ^ Trying a movement that the profession does not allow
 | CaptureByTam -- ^ Trying to capture a piece by Tam2
 | Tam2PrivilegeExceeded{
  _from :: Square,
  _thru :: (Maybe Square),
  _to :: Square
  } -- ^ Trying a movement that the Tam2 cannot do
  deriving(Show, Eq, Ord) 

{-
********************************************
* Primitive operations on the board 
********************************************
-}
-- | The list of squares that are inherently tam2Hue
inherentTam2Hue :: [Square]
inherentTam2Hue = [sqNI, sqNAI, sqTU, sqTY, sqZO, sqXU, sqXY, sqCI, sqCAI]

getNeighborsAndSelf :: Square -> [Square]
getNeighborsAndSelf sq = mapMaybe (`add` sq) [Vec a b | a <- [-1,0,1], b <- [-1,0,1]]

getNeighbors :: Square -> [Square]
getNeighbors sq = mapMaybe (`add` sq) 
 [Vec a b | (a,b) <- [(1,1),(1,-1),(-1,1),(-1,-1),(0,1),(1,0),(0,-1),(-1,0)]]

isNeighborOf :: Square -> Square -> Bool
isNeighborOf s1 s2 = s1 `elem` getNeighbors s2

-- | Checks whether the piece on a given square is a Tam2HueAUai1 that belong to the side.
isTam2HueAUai1 :: Side -> Board1 -> Square -> Bool
isTam2HueAUai1 sid board sq = isJust $ do
 piece <- sq `M.lookup` board
 (_,Uai1,s) <- toPhantom piece
 guard(s == sid && isTam2Hue board sq)

-- | Checks whether a given square is in Tam2Hue.
isTam2Hue :: Board1 -> Square -> Bool
isTam2Hue board sq = isJust $
 if sq `elem` inherentTam2Hue
  then return ()
  else let ps = mapMaybe (`M.lookup` board) $ getNeighborsAndSelf sq in
   unless (phantomTam `elem` map toPhantom ps) Nothing

-- | Returns whether the square is occupied
isOccupied :: Square -> Board1 -> Bool
isOccupied = M.member

-- | Puts a piece on a square. Fails with 'AlreadyOccupied' if already occupied.
putPiece :: Piece -> Square -> Board1 -> Either Error Board1
putPiece p sq b = if sq `isOccupied` b then Left (AlreadyOccupied sq) else Right(M.insert sq p b)

-- | Removes a piece. Fails with 'EmptySquare' if the specified square is empty.
removePiece :: Square -> Board1 -> Either Error (Piece, Board1)
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

-- | Moves a piece from a square to a square, returning the phantom version of the piece that was moved. Raises: 
--
--   * 'EmptySquare' if the original square is empty
--   * 'AlreadyOccupied' if the resulting square is already occupied
movePieceFromToFull :: Square -> Square -> Board1 -> Either Error (Maybe PhantomPiece, Board1)
movePieceFromToFull from to b = do
 (p, new_b) <- removePiece from b
 newerBoard <- putPiece p to new_b
 return (toPhantom p, newerBoard)

-- | The list of squares, arranged in the following order: @['sqKA', 'sqLA', 'sqNA' ... 'sqCIA', 'sqMIA', 'sqPIA']@.
sqList :: [Square]
sqList = [Square r c | r <-[RowA ..RowIA],c <-[ColumnK .. ColumnP]]

sqKA,  sqLA,  sqNA,  sqTA,  sqZA,  sqXA,  sqCA,  sqMA,  sqPA, 
 sqKE,  sqLE,  sqNE,  sqTE,  sqZE,  sqXE,  sqCE,  sqME,  sqPE, 
 sqKI,  sqLI,  sqNI,  sqTI,  sqZI,  sqXI,  sqCI,  sqMI,  sqPI, 
 sqKU,  sqLU,  sqNU,  sqTU,  sqZU,  sqXU,  sqCU,  sqMU,  sqPU, 
 sqKO,  sqLO,  sqNO,  sqTO,  sqZO,  sqXO,  sqCO,  sqMO,  sqPO, 
 sqKY,  sqLY,  sqNY,  sqTY,  sqZY,  sqXY,  sqCY,  sqMY,  sqPY, 
 sqKAI, sqLAI, sqNAI, sqTAI, sqZAI, sqXAI, sqCAI, sqMAI, sqPAI,
 sqKAU, sqLAU, sqNAU, sqTAU, sqZAU, sqXAU, sqCAU, sqMAU, sqPAU,
 sqKIA, sqLIA, sqNIA, sqTIA, sqZIA, sqXIA, sqCIA, sqMIA, sqPIA :: Square
[
 sqKA,  sqLA,  sqNA,  sqTA,  sqZA,  sqXA,  sqCA,  sqMA,  sqPA, 
 sqKE,  sqLE,  sqNE,  sqTE,  sqZE,  sqXE,  sqCE,  sqME,  sqPE, 
 sqKI,  sqLI,  sqNI,  sqTI,  sqZI,  sqXI,  sqCI,  sqMI,  sqPI, 
 sqKU,  sqLU,  sqNU,  sqTU,  sqZU,  sqXU,  sqCU,  sqMU,  sqPU, 
 sqKO,  sqLO,  sqNO,  sqTO,  sqZO,  sqXO,  sqCO,  sqMO,  sqPO, 
 sqKY,  sqLY,  sqNY,  sqTY,  sqZY,  sqXY,  sqCY,  sqMY,  sqPY, 
 sqKAI, sqLAI, sqNAI, sqTAI, sqZAI, sqXAI, sqCAI, sqMAI, sqPAI,
 sqKAU, sqLAU, sqNAU, sqTAU, sqZAU, sqXAU, sqCAU, sqMAU, sqPAU,
 sqKIA, sqLIA, sqNIA, sqTIA, sqZIA, sqXIA, sqCIA, sqMIA, sqPIA] = sqList

