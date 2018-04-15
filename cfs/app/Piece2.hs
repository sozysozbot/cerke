
module Piece2
(Color(..)
,Side(..)
,Piece(..)
,Profession(..)
,profToLin
,ProfOrTam
,flipSide
,PhantomPiece
,match
,getSide,getProf
,船, 兵, 弓, 車, 虎, 馬, 筆, 巫, 将, 王
) where
data Color = Kok1 | Huok2 deriving(Show, Eq, Ord)
data Side = Upward | Downward deriving(Show, Eq, Ord)
data Piece = Tam2 | Piece {color :: Color, prof :: Profession, side :: Side} deriving(Show, Eq, Ord)
data Profession = Nuak1 | Kauk2 | Gua2 | Kaun1 | Dau2 | Maun1 | Kua2 | Tuk2 | Uai1 | Io deriving(Show, Eq, Ord, Enum)
type ProfOrTam = Either () Profession -- Left () is tam2

船, 兵, 弓, 車, 虎, 馬, 筆, 巫, 将, 王 :: Profession
船 = Nuak1
兵 = Kauk2
弓 = Gua2
車 = Kaun1
虎 = Dau2
馬 = Maun1
筆 = Kua2
巫 = Tuk2
将 = Uai1
王 = Io

profToLin :: Profession -> Char
profToLin p = "船兵弓車虎馬筆巫将王" !! fromEnum p

getSide :: Piece -> Maybe Side
getSide Tam2 = Nothing
getSide (Piece _ _ s) = Just s

getProf :: Piece -> Maybe Profession
getProf Tam2 = Nothing
getProf (Piece _ p _) = Just p

flipSide :: Piece -> Maybe Piece
flipSide Tam2 = Nothing
flipSide piece = Just $ piece{side = flipSide'(side piece)}

flipSide' :: Side -> Side
flipSide' Upward = Downward
flipSide' Downward = Upward

type PhantomPiece = (Color, Profession, Side) -- Not a physical piece; only serves to be a template

match :: PhantomPiece -> Piece -> Bool
match (_,_,_) Tam2 = False
match (c,p,s) Piece{color=co, prof=pr, side=si} = (c==co) && (p==pr) && (s==si)
