module Board
(Col(..)
,Row(..)
,Square(..)
,Board1
,Vec(..)
,putPiece
,removePiece
,movePiece
,loadBoard
,drawBoard
,movePieceFromTo
) where
import Piece
import qualified Data.Map as M
import Data.Char
import Control.Monad

data Col = CK | CL | CN | CT | CZ | CX | CC | CM | CP deriving(Show, Eq, Ord, Enum)
data Row = RA | RE | RI | RU | RO | RY | RAI | RAU | RIA deriving(Show,Eq, Ord,Enum)
data Square = Square{row :: Row, col :: Col} deriving(Eq, Ord)

instance Show Square where
 show (Square{col=c,row=r}) = tail(show c) ++ tail(show r)

type Board1 = M.Map Square Piece

data Vec = Vec{dx :: Int, dy :: Int}

add :: Vec -> Square -> Maybe Square
add (Vec x y) (Square{col=c,row=r}) = do
 new_c <- add' x c
 new_r <- add' y r
 return Square{col=new_c, row=new_r}

add' :: (Enum a) => Int -> a -> Maybe a
add' a c
 | a+b < 0 = Nothing  
 | a+b > 8 = Nothing  
 | otherwise = Just $ toEnum (a+b)
 where b = fromEnum c



{-
********************************************
* Primitive operations on the board 
********************************************
-}

isOccupiedFor :: Square -> Board1 -> Bool
isOccupiedFor = M.member

-- put a piece on a square. fails if already occupied.
putPiece :: Piece -> Square -> Board1 -> Maybe Board1
putPiece p sq b = if sq `isOccupiedFor` b then Nothing else Just(M.insert sq p b)

-- remove a piece. fails if empty.
removePiece :: Square -> Board1 -> Maybe (Piece, Board1)
removePiece sq b = do
 p <- sq `M.lookup` b
 return (p, M.delete sq b)

-- move a piece on a square according to the vector. fails if:
--  the piece goes out of the board
--  the original square is empty
--  the resulting square is already occupied
movePiece :: Vec -> Square -> Board1 -> Maybe Board1
movePiece vec sq b = do
 (p, new_b) <- removePiece sq b
 new_sq <- add vec sq
 putPiece p new_sq new_b

movePieceFromTo :: Square -> Square -> Board1 -> Maybe Board1
movePieceFromTo from to b = do
 (p, new_b) <- removePiece from b
 putPiece p to new_b

{-
*******************
* Loading the board
*******************
-}
-- Alpha "^" Beta "#" Gamma "h" Alpha
data Stat = Alpha | Beta Side | Gamma Side ProfOrTam

sqList :: [Square]
sqList = [Square r c | r <-[RA ..RIA],c <-[CK .. CP]]

loadBoard :: String -> Maybe Board1
loadBoard str = do
 pieces <- load Alpha str
 guard (length pieces == 81)
 return $ M.fromList $ [ (sq, p) | (sq, Just p) <- zip sqList pieces]

load :: Stat -> String -> Maybe [Maybe Piece]
load a s@(x:_)
 | isSpace x = load a (dropWhile isSpace s)

load Alpha ('^':xs) = load (Beta Upward) xs
load Alpha ('_':xs) = load (Beta Downward) xs
load Alpha ('-':xs) = (Nothing :) <$> load Alpha xs
load Alpha "" = Just []
load Alpha _ = Nothing

load (Beta _) "" = Nothing
load (Beta side_) (k:xs) = do
 profOrTam <- asciiToProf k
 load (Gamma side_ profOrTam) xs

load (Gamma _ _) "" = Nothing
load (Gamma side_ profOrTam) (c:xs) = do
 color_ <- asciiToColor c
 let piece = case profOrTam of{
   Right prof_ -> Piece{color=color_, side=side_, prof=prof_};
   Left () -> Tam2
 }
 (Just piece :) <$> load Alpha xs

{-
*******************
* Drawing the board
*******************
-}

drawBoard :: Board1 -> String
drawBoard b = foo [ convert(M.lookup sq b) | sq <- sqList]
 where 
  foo [] = ""
  foo arr = let (a,b) = splitAt 9 arr in concat a ++ "\n" ++ foo b

convert :: Maybe Piece -> String
convert Nothing = " - "
convert (Just Tam2) = "^$h"
convert (Just Piece{color=c,prof=p,side=s}) = [sideToAscii s, profToAscii p ,colorToAscii c]

