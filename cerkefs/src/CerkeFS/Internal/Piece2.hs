module CerkeFS.Internal.Piece2
(Color(..)
,Side(..)
,Piece(..)
,Profession(..)
,ProfOrTam
) where
data Color = Kok1 | Huok2 deriving(Show, Eq, Ord)
data Side = Upward | Downward deriving(Show, Eq, Ord)
data Piece = Tam2 | Piece {color :: Color, prof :: Profession, side :: Side} deriving(Show, Eq, Ord)
data Profession = Nuak1 | Kauk2 | Gua2 | Kaun1 | Dau2 | Maun1 | Kua2 | Tuk2 | Uai1 | Io deriving(Show, Eq, Ord, Enum)
type ProfOrTam = Either () Profession -- Left () is tam2
