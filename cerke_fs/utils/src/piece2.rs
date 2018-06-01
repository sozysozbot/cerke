#[derive(Debug, PartialEq, Eq)]
pub enum Profession {
    /// Vessel, 船, felkana
    Nuak1,
    /// Pawn, 兵, elmer
    Kauk2,
    /// Rook, 弓, gustuer
    Gua2,
    /// Bishop, 車, vadyrd
    Kaun1,
    /// Tiger, 虎, stistyst
    Dau2,
    /// Horse, 馬, dodor  
    Maun1,
    /// Clerk, 筆, kua
    Kua2,
    /// Shaman, 巫, terlsk  
    Tuk2,
    /// General, 将, varxle
    Uai1,
    /// King, 王, ales
    Io,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Color {
    /// Red, 赤
    Kok1,
    /// Black, 黒
    Huok2,
}

#[derive(Debug)]
pub enum Side {
    /// Pieces that points upward. Denoted by @^@ in the ASCII notation.
    Upward,
    /// Pieces that points downward. Denoted by @_@ in the ASCII notation.
    Downward,
}

#[derive(Debug)]
pub enum Piece {
    /// Minds, 皇, tam
    Tam2,
    NonTam2Piece {
        /// The color of the piece
        color: Color,
        /// The profession of the piece
        prof: Profession,
        /// The side that the piece belongs to
        side: Side,
    },
}
