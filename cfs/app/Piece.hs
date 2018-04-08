
module Piece
(Color(..)
,Side(..)
,Piece(..)
,Profession(..)
,profToAscii
,profToLin
,asciiToProf
,asciiToColor
,colorToAscii
,sideToAscii
,ProfOrTam
) where
data Color = Kok1 | Huok2 deriving(Show, Eq, Ord)
data Side = Upward | Downward deriving(Show, Eq, Ord)
data Piece = Tam2 | Piece {color :: Color, prof :: Profession, side :: Side} deriving(Show, Eq, Ord)
data Profession = Nuak1 | Kauk2 | Gua2 | Kaun1 | Dau2 | Maun1 | Kua2 | Tuk2 | Uai1 | Io deriving(Show, Eq, Ord, Enum)
type ProfOrTam = Either () Profession -- Left () is tam2

profToAscii :: Profession -> Char
profToAscii Nuak1 = '!'
profToAscii Io = '#'
profToAscii p = toEnum $ fromEnum '0' + fromEnum p

profToLin :: Profession -> Char
profToLin p = "船兵弓車虎馬筆巫将王" !! fromEnum p

asciiToProf :: Char -> Maybe (ProfOrTam) -- Left () is tam2
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

