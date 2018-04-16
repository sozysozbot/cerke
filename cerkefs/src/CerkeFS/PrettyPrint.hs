module CerkeFS.PrettyPrint
(loadBoard
,drawBoard
,initialBoard
,sqKA,  sqLA,  sqNA,  sqTA,  sqZA,  sqXA,  sqCA,  sqMA,  sqPA, 
 sqKE,  sqLE,  sqNE,  sqTE,  sqZE,  sqXE,  sqCE,  sqME,  sqPE, 
 sqKI,  sqLI,  sqNI,  sqTI,  sqZI,  sqXI,  sqCI,  sqMI,  sqPI, 
 sqKU,  sqLU,  sqNU,  sqTU,  sqZU,  sqXU,  sqCU,  sqMU,  sqPU, 
 sqKO,  sqLO,  sqNO,  sqTO,  sqZO,  sqXO,  sqCO,  sqMO,  sqPO, 
 sqKY,  sqLY,  sqNY,  sqTY,  sqZY,  sqXY,  sqCY,  sqMY,  sqPY, 
 sqKAI, sqLAI, sqNAI, sqTAI, sqZAI, sqXAI, sqCAI, sqMAI, sqPAI,
 sqKAU, sqLAU, sqNAU, sqTAU, sqZAU, sqXAU, sqCAU, sqMAU, sqPAU,
 sqKIA, sqLIA, sqNIA, sqTIA, sqZIA, sqXIA, sqCIA, sqMIA, sqPIA
) where
import CerkeFS.Internal.Piece2
import CerkeFS.Board
import qualified Data.Map as M
import Data.Char
import Control.Monad
import Data.Maybe

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

sqList :: [Square]
sqList = [Square r c | r <-[RA ..RIA],c <-[CK .. CP]]

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
drawBoard :: Board1 -> String
drawBoard b = foo [ convert(M.lookup sq b) | sq <- sqList]
 where 
  foo [] = ""
  foo arr = let (a,c) = splitAt 9 arr in concat a ++ "\n" ++ foo c

convert :: Maybe Piece -> String
convert Nothing = " - "
convert (Just Tam2) = "^$h"
convert (Just Piece{color=c,prof=p,side=s}) = [sideToAscii s, profToAscii p ,colorToAscii c]


{-
************
* default
************
-}
initialBoard :: Board1
initialBoard = fromJust $ loadBoard
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\
 \_7k_2k - _4k - _4h - _2h_7h\
 \_1h_1k_1h_1k_!k_1k_1h_1k_1h\
 \ -  -  -  -  -  -  -  -  - \
 \ -  -  -  - ^$h -  -  -  -\
 \ -  -  -  -  -  -  -  -  - \
 \^1h^1k^1h^1k^!h^1k^1h^1k^1h\
 \^7h^2h - ^4h - ^4k - ^2k^7k\
 \^6k^5k^3k^8k^#h^8h^3h^5h^6h"

{-
initialBoard = M.fromList [(Square {row = RA, col = CK},Piece {color = Huok2, prof = Kua2, side = Downward}),(Square {row = RA, col = CL},Piece {color = Huok2, prof = Maun1, side = Downward}),(Square {row = RA, col = CN},Piece {color = Huok2, prof = Kaun1, side = Downward}),(Square {row = RA, col = CT},Piece {color = Huok2, prof = Uai1, side = Downward}),(Square {row = RA, col = CZ},Piece {color = Kok1, prof = Io, side = Downward}),(Square {row = RA, col = CX},Piece {color = Kok1, prof = Uai1, side = Downward}),(Square {row = RA, col = CC},Piece {color = Kok1, prof = Kaun1, side = Downward}),(Square {row = RA, col = CM},Piece {color = Kok1, prof = Maun1, side = Downward}),(Square {row = RA, col = CP},Piece {color = Kok1, prof = Kua2, side = Downward}),(Square {row = RE, col = CK},Piece {color = Kok1, prof = Tuk2, side = Downward}),(Square {row = RE, col = CL},Piece {color = Kok1, prof = Gua2, side = Downward}),(Square {row = RE, col = CT},Piece {color = Kok1, prof = Dau2, side = Downward}),(Square {row = RE, col = CX},Piece {color = Huok2, prof = Dau2, side = Downward}),(Square {row = RE, col = CM},Piece {color = Huok2, prof = Gua2, side = Downward}),(Square {row = RE, col = CP},Piece {color = Huok2, prof = Tuk2, side = Downward}),(Square {row = RI, col = CK},Piece {color = Huok2, prof = Kauk2, side = Downward}),(Square {row = RI, col = CL},Piece {color = Kok1, prof = Kauk2, side = Downward}),(Square {row = RI, col = CN},Piece {color = Huok2, prof = Kauk2, side = Downward}),(Square {row = RI, col = CT},Piece {color = Kok1, prof = Kauk2, side = Downward}),(Square {row = RI, col = CZ},Piece {color = Kok1, prof = Nuak1, side = Downward}),(Square {row = RI, col = CX},Piece {color = Kok1, prof = Kauk2, side = Downward}),(Square {row = RI, col = CC},Piece {color = Huok2, prof = Kauk2, side = Downward}),(Square {row = RI, col = CM},Piece {color = Kok1, prof = Kauk2, side = Downward}),(Square {row = RI, col = CP},Piece {color = Huok2, prof = Kauk2, side = Downward}),(Square {row = RO, col = CZ},Tam2),(Square {row = RAI, col = CK},Piece {color = Huok2, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CL},Piece {color = Kok1, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CN},Piece {color = Huok2, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CT},Piece {color = Kok1, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CZ},Piece {color = Huok2, prof = Nuak1, side = Upward}),(Square {row = RAI, col = CX},Piece {color = Kok1, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CC},Piece {color = Huok2, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CM},Piece {color = Kok1, prof = Kauk2, side = Upward}),(Square {row = RAI, col = CP},Piece {color = Huok2, prof = Kauk2, side = Upward}),(Square {row = RAU, col = CK},Piece {color = Huok2, prof = Tuk2, side = Upward}),(Square {row = RAU, col = CL},Piece {color = Huok2, prof = Gua2, side = Upward}),(Square {row = RAU, col = CT},Piece {color = Huok2, prof = Dau2, side = Upward}),(Square {row = RAU, col = CX},Piece {color = Kok1, prof = Dau2, side = Upward}),(Square {row = RAU, col = CM},Piece {color = Kok1, prof = Gua2, side = Upward}),(Square {row = RAU, col = CP},Piece {color = Kok1, prof = Tuk2, side = Upward}),(Square {row = RIA, col = CK},Piece {color = Kok1, prof = Kua2, side = Upward}),(Square {row = RIA, col = CL},Piece {color = Kok1, prof = Maun1, side = Upward}),(Square {row = RIA, col = CN},Piece {color = Kok1, prof = Kaun1, side = Upward}),(Square {row = RIA, col = CT},Piece {color = Kok1, prof = Uai1, side = Upward}),(Square {row = RIA, col = CZ},Piece {color = Huok2, prof = Io, side = Upward}),(Square {row = RIA, col = CX},Piece {color = Huok2, prof = Uai1, side = Upward}),(Square {row = RIA, col = CC},Piece {color = Huok2, prof = Kaun1, side = Upward}),(Square {row = RIA, col = CM},Piece {color = Huok2, prof = Maun1, side = Upward}),(Square {row = RIA, col = CP},Piece {color = Huok2, prof = Kua2, side = Upward})]
-}
