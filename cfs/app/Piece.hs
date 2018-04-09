
module Piece
(Color(..)
,Side(..)
,Piece(..)
,Profession(..)
,profToLin
,ProfOrTam
,flipSide
,PhantomPiece
,match
) where
data Color = Kok1 | Huok2 deriving(Show, Eq, Ord)
data Side = Upward | Downward deriving(Show, Eq, Ord)
data Piece = Tam2 | Piece {color :: Color, prof :: Profession, side :: Side} deriving(Show, Eq, Ord)
data Profession = Nuak1 | Kauk2 | Gua2 | Kaun1 | Dau2 | Maun1 | Kua2 | Tuk2 | Uai1 | Io deriving(Show, Eq, Ord, Enum)
type ProfOrTam = Either () Profession -- Left () is tam2

profToLin :: Profession -> Char
profToLin p = "船兵弓車虎馬筆巫将王" !! fromEnum p

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
