module CerkeFS.PrettyPrint
(loadBoard
,drawBoard
,convertPieceToStr
) where
import CerkeFS.Internal.Piece2
import CerkeFS.Board2
import Data.Char
import Control.Monad
import Data.Maybe(maybe)

asciiToProf :: Char -> Maybe ProfOrTam -- Left () is tam2
asciiToProf '!' = Just $ Right Nuak1
asciiToProf '1' = Just $ Right Kauk2  
asciiToProf '2' = Just $ Right Gua2 
asciiToProf '3' = Just $ Right Kaun1
asciiToProf '4' = Just $ Right Dau2 
asciiToProf '5' = Just $ Right Maun1
asciiToProf '6' = Just $ Right Kua2 
asciiToProf '7' = Just $ Right Tuk2 
asciiToProf '8' = Just $ Right Uai1
asciiToProf '#' = Just $ Right Io
asciiToProf '$' = Just $ Left ()
asciiToProf _ = Nothing

profToAscii :: Profession -> Char
profToAscii Nuak1 = '!'
profToAscii Io = '#'
profToAscii p = toEnum $ fromEnum '0' + fromEnum p

asciiToColor :: Char -> Maybe Color
asciiToColor 'h' = Just Huok2
asciiToColor 'k' = Just Kok1
asciiToColor _   = Nothing

colorToAscii :: Color -> Char
colorToAscii Huok2 = 'h'
colorToAscii Kok1  = 'k'

sideToAscii :: Side -> Char
sideToAscii Upward   = '^'
sideToAscii Downward = '_'

type ProfOrTam = Either () Profession -- Left () is tam2

{-
*******************
* Loading the board
*******************
-}
-- Alpha "^" Beta "#" Gamma "h" Alpha
data Stat = Alpha | Beta Side | Gamma Side ProfOrTam



-- | Converts the ASCII board format used in the <https://sozysozbot.github.io/cerke/ Cerke board image generator> to the 'Board1'.
loadBoard :: String -> Maybe Board1
loadBoard str = do
 pieces <- load Alpha str
 guard (length pieces == 81)
 return $ fromBoard1_old [ (sq, p) | (sq, Just p) <- zip sqList pieces]

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
   Right prof_ -> NonTam2Piece $ Piece{color=color_, side=side_, prof=prof_};
   Left () -> Tam2
 }
 (Just piece :) <$> load Alpha xs


{-
*******************
* Drawing the board
*******************
-}
-- | Converts the 'Board1' to the ASCII board format used in the <https://sozysozbot.github.io/cerke/ Cerke board image generator>.
drawBoard :: Board1 -> String
drawBoard b = foo [ maybe " - " convertPieceToStr (lookup_ sq b) | sq <- sqList]
 where 
  foo [] = ""
  foo arr = let (a,c) = splitAt 9 arr in concat a ++ "\n" ++ foo c

-- | Converts the 'Piece' to the ASCII format used in the <https://sozysozbot.github.io/cerke/ Cerke board image generator>.
convertPieceToStr :: Piece -> String
convertPieceToStr Tam2 = "^$h"
convertPieceToStr (NonTam2Piece Piece{color=c,prof=p,side=s}) = [sideToAscii s, profToAscii p ,colorToAscii c]

