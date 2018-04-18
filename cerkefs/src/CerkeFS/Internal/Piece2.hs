module CerkeFS.Internal.Piece2
(Color(..)
,Side(..)
,Piece(..)
,Profession(..)
) where
data Color
 = Kok1  -- ^ Red, 赤
 | Huok2 -- ^ Black, 黒
 deriving(Show, Eq, Ord)
data Side
 = Upward -- ^ Pieces that points upward. Denoted by @^@ in the ASCII notation.
 | Downward-- ^ Pieces that points downward. Denoted by @_@ in the ASCII notation.
 deriving(Show, Eq, Ord)
data Piece 
 = Tam2 -- ^ Minds, 皇, tam
 | Piece {
  color :: Color, -- ^ The color of the piece
  prof :: Profession, -- ^ The profession of the piece
  side :: Side -- ^ The side that the piece belongs to
 } deriving(Show, Eq, Ord)
data Profession
 = Nuak1 -- ^ Vessel, 船, felkana  
 | Kauk2 -- ^ Pawn, 兵, elmer
 | Gua2  -- ^ Rook, 弓, gustuer
 | Kaun1 -- ^ Bishop, 車, vadyrd
 | Dau2  -- ^ Tiger, 虎, stistyst
 | Maun1 -- ^ Horse, 馬, dodor  
 | Kua2  -- ^ Clerk, 筆, kua 
 | Tuk2  -- ^ Shaman, 巫, terlsk  
 | Uai1  -- ^ General, 将, varxle
 | Io    -- ^ King, 王, ales
 deriving(Show, Eq, Ord, Enum)
