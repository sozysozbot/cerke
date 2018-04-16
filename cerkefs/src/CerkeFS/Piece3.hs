module CerkeFS.Piece3
(Color(..)
,Side(..)
,Piece() -- hide the constructor
,Profession(..)
,profToLin
,ProfOrTam
,flipSide
,PhantomPiece
,match
,getSide
,toPhantom
,船, 兵, 弓, 車, 虎, 馬, 筆, 巫, 将, 王
,黒, 赤
) where
import CerkeFS.Internal.Piece2

type PhantomPiece = (Color, Profession, Side) -- Not a physical piece; only serves to be a template

-- | Flip the side that the piece belongs to, or 'Nothing' if 'Tam2'.
flipSide :: Piece -> Maybe Piece
flipSide Tam2 = Nothing
flipSide piece = Just $ piece{side = flipSide'(side piece)}

flipSide' :: Side -> Side
flipSide' Upward = Downward
flipSide' Downward = Upward

-- | Converts 'Piece' into 'PhantomPiece'.
toPhantom :: Piece -> Maybe PhantomPiece
toPhantom Tam2 = Nothing
toPhantom (Piece c p s) = Just(c,p,s)

match :: PhantomPiece -> Piece -> Bool
match (_,_,_) Tam2 = False
match (c,p,s) Piece{color=co, prof=pr, side=si} = (c==co) && (p==pr) && (s==si)

getSide :: Piece -> Maybe Side
getSide Tam2 = Nothing
getSide (Piece _ _ s) = Just s

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

黒, 赤 :: Color
黒 = Huok2
赤 = Kok1

profToLin :: Profession -> Char
profToLin p = "船兵弓車虎馬筆巫将王" !! fromEnum p

