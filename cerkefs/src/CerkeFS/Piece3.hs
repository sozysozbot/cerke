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

-- | An alias for 'Nuak1'.
船 :: Profession 
船 = Nuak1

-- | An alias for 'Kauk2'.
兵 :: Profession
兵 = Kauk2 

-- | An alias for 'Gua2'. 
弓 :: Profession
弓 = Gua2

-- | An alias for 'Kaun1'.
車 :: Profession
車 = Kaun1

-- | An alias for 'Dau2'. 
虎 :: Profession
虎 = Dau2  

-- | An alias for 'Maun1'.
馬 :: Profession
馬 = Maun1

-- | An alias for 'Kua2'. 
筆 :: Profession
筆 = Kua2 

-- | An alias for 'Tuk2'. 
巫 :: Profession
巫 = Tuk2 

-- | An alias for 'Uai1'. 
将 :: Profession 
将 = Uai1 

-- | An alias for 'Io'.   
王 :: Profession 
王 = Io   

-- | An alias for 'Huok2'.
黒 :: Color
黒 = Huok2

-- | An alias for 'Kok1'.
赤 :: Color
赤 = Kok1

profToLin :: Profession -> Char
profToLin p = "船兵弓車虎馬筆巫将王" !! fromEnum p

