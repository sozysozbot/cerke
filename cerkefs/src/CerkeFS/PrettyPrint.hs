module CerkeFS.PrettyPrint
(loadBoard
,drawBoard
,initialBoard
) where
import CerkeFS.Internal.Piece2
import CerkeFS.Board2
import qualified Data.Map as M
import Data.Char
import Control.Monad

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
 return $ M.fromList [ (sq, p) | (sq, Just p) <- zip sqList pieces]

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
-- | Converts the 'Board1' to the ASCII board format used in the <https://sozysozbot.github.io/cerke/ Cerke board image generator>.
drawBoard :: Board1 -> String
drawBoard b = foo [ convert(M.lookup sq b) | sq <- sqList]
 where 
  foo [] = ""
  foo arr = let (a,c) = splitAt 9 arr in concat a ++ "\n" ++ foo c

convert :: Maybe Piece -> String
convert Nothing = " - "
convert (Just Tam2) = "^$h"
convert (Just Piece{color=c,prof=p,side=s}) = [sideToAscii s, profToAscii p ,colorToAscii c]


-- | Initial configuration of cerke board.
initialBoard :: Board1
initialBoard = M.fromList [(sqKA,Piece {color = Huok2, prof = Kua2, side = Downward}),(sqLA,Piece {color = Huok2, prof = Maun1, side = Downward}),(sqNA,Piece {color = Huok2, prof = Kaun1, side = Downward}),(sqTA,Piece {color = Huok2, prof = Uai1, side = Downward}),(sqZA,Piece {color = Kok1, prof = Io, side = Downward}),(sqXA,Piece {color = Kok1, prof = Uai1, side = Downward}),(sqCA,Piece {color = Kok1, prof = Kaun1, side = Downward}),(sqMA,Piece {color = Kok1, prof = Maun1, side = Downward}),(sqPA,Piece {color = Kok1, prof = Kua2, side = Downward}),(sqKE,Piece {color = Kok1, prof = Tuk2, side = Downward}),(sqLE,Piece {color = Kok1, prof = Gua2, side = Downward}),(sqTE,Piece {color = Kok1, prof = Dau2, side = Downward}),(sqXE,Piece {color = Huok2, prof = Dau2, side = Downward}),(sqME,Piece {color = Huok2, prof = Gua2, side = Downward}),(sqPE,Piece {color = Huok2, prof = Tuk2, side = Downward}),(sqKI,Piece {color = Huok2, prof = Kauk2, side = Downward}),(sqLI,Piece {color = Kok1, prof = Kauk2, side = Downward}),(sqNI,Piece {color = Huok2, prof = Kauk2, side = Downward}),(sqTI,Piece {color = Kok1, prof = Kauk2, side = Downward}),(sqZI,Piece {color = Kok1, prof = Nuak1, side = Downward}),(sqXI,Piece {color = Kok1, prof = Kauk2, side = Downward}),(sqCI,Piece {color = Huok2, prof = Kauk2, side = Downward}),(sqMI,Piece {color = Kok1, prof = Kauk2, side = Downward}),(sqPI,Piece {color = Huok2, prof = Kauk2, side = Downward}),(sqZO,Tam2),(sqKAI,Piece {color = Huok2, prof = Kauk2, side = Upward}),(sqLAI,Piece {color = Kok1, prof = Kauk2, side = Upward}),(sqNAI,Piece {color = Huok2, prof = Kauk2, side = Upward}),(sqTAI,Piece {color = Kok1, prof = Kauk2, side = Upward}),(sqZAI,Piece {color = Huok2, prof = Nuak1, side = Upward}),(sqXAI,Piece {color = Kok1, prof = Kauk2, side = Upward}),(sqCAI,Piece {color = Huok2, prof = Kauk2, side = Upward}),(sqMAI,Piece {color = Kok1, prof = Kauk2, side = Upward}),(sqPAI,Piece {color = Huok2, prof = Kauk2, side = Upward}),(sqKAU,Piece {color = Huok2, prof = Tuk2, side = Upward}),(sqLAU,Piece {color = Huok2, prof = Gua2, side = Upward}),(sqTAU,Piece {color = Huok2, prof = Dau2, side = Upward}),(sqXAU,Piece {color = Kok1, prof = Dau2, side = Upward}),(sqMAU,Piece {color = Kok1, prof = Gua2, side = Upward}),(sqPAU,Piece {color = Kok1, prof = Tuk2, side = Upward}),(sqKIA,Piece {color = Kok1, prof = Kua2, side = Upward}),(sqLIA,Piece {color = Kok1, prof = Maun1, side = Upward}),(sqNIA,Piece {color = Kok1, prof = Kaun1, side = Upward}),(sqTIA,Piece {color = Kok1, prof = Uai1, side = Upward}),(sqZIA,Piece {color = Huok2, prof = Io, side = Upward}),(sqXIA,Piece {color = Huok2, prof = Uai1, side = Upward}),(sqCIA,Piece {color = Huok2, prof = Kaun1, side = Upward}),(sqMIA,Piece {color = Huok2, prof = Maun1, side = Upward}),(sqPIA,Piece {color = Huok2, prof = Kua2, side = Upward})]

