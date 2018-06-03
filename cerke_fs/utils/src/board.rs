use board::Error::AlreadyOccupied;
use board::Error::EmptySquare;
use piece2::to_phantom;
use piece2::PhantomPiece;
use piece2::Piece;
use piece2::Profession;
use piece2::Profession::Uai1;
use piece2::Side;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
#[derive(Debug)]

pub enum Col {
    ColumnK,
    ColumnL,
    ColumnN,
    ColumnT,
    ColumnZ,
    ColumnX,
    ColumnC,
    ColumnM,
    ColumnP,
}

#[derive(Debug)]
pub enum Row {
    RowA,
    RowE,
    RowI,
    RowU,
    RowO,
    RowY,
    RowAI,
    RowAU,
    RowIA,
}

#[derive(Debug)]
pub struct Square {
    pub row: Row,
    pub col: Col,
}

pub struct Board1 {
    internal: [Option<Piece>; 81],
}

#[derive(Copy, Clone)]
pub struct Square2 {
    num: usize,
}
impl Square2 {
    pub fn get_num(&self) -> usize {
        self.num
    }
    pub fn from_num(u: usize) -> Option<Square2> {
        if u < 81 {
            Some(Square2 { num: u })
        } else {
            None
        }
    }
}

impl Debug for Square2 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (r, c) = (self.get_num() / 9, self.get_num() % 9);
        let str_r = match r {
            0 => "A",
            1 => "E",
            2 => "I",
            3 => "U",
            4 => "O",
            5 => "Y",
            6 => "AI",
            7 => "AU",
            8 => "IA",
            _ => panic!("Cannot happen: uhijkmrsdf"),
        };
        let str_c = match c {
            0 => "K",
            1 => "L",
            2 => "N",
            3 => "T",
            4 => "Z",
            5 => "X",
            6 => "C",
            7 => "M",
            8 => "P",
            _ => panic!("Cannot happen: bgdfgdrfgcdf"),
        };

        write!(f, "sq{}{}", str_c, str_r)
    }
}

impl Board1 {
    /// Returns whether the square is occupied
    pub fn is_occupied(&self, sq: Square2) -> bool {
        match self.internal[sq.get_num()] {
            None => true,
            _ => false,
        }
    }

    ///Puts a piece on a square. Fails with AlreadyOccupied if already occupied.
    pub fn put_piece<'a>(mut self, p: Piece, sq: Square2) -> Result<Self, Error> {
        if self.is_occupied(sq) {
            Err(AlreadyOccupied(sq))
        } else {
            self.internal[sq.get_num()] = Some(p);
            Ok(self)
        }
    }

    ///Removes a piece. Fails with EmptySquare if the specified square is empty.
    pub fn remove_piece<'a>(mut self, sq: Square2) -> Result<(Piece, Self), Error> {
        let opt_p = self.internal[sq.get_num()].clone();
        match opt_p {
            Some(p) => {
                self.internal[sq.get_num()] = None;
                Ok((p, self))
            }
            None => Err(EmptySquare(sq)),
        }
    }

    /// Moves a piece from a square to a square, returning the phantom version of the piece that was moved. Raises:
    /// EmptySquare if the original square is empty
    /// AlreadyOccupied if the resulting square is already occupied
    pub fn move_piece_from_to_full(
        self,
        from: Square2,
        to: Square2,
    ) -> Result<(Option<PhantomPiece>, Board1), Error> {
        let (p, new_self) = self.remove_piece(from)?;
        let phantom = to_phantom(&p);
        let newer_board = new_self.put_piece(p, to)?;
        Ok((phantom, newer_board))
    }
    /// Checks whether a given square is in Tam2Hue.
    pub fn is_tam2_hue(&self, sq: Square2) -> bool {
        if is_inherent_tam2_hue(sq) {
            return true;
        }

        for neighbor in get_neighbors_and_self(sq) {
            match self.internal[neighbor.get_num()] {
                None => continue,
                Some(ref piece) => match piece {
                    Piece::Tam2 => return true, // Tam2 exists
                    _ => continue,
                },
            }
        }
        false
    }

    /// Checks whether the piece on a given square is a Tam2HueAUai1 that belong to the side.
    pub fn is_tam2_hue_a_uai1(&self, sid: Side, sq: Square2) -> bool {
        if let Some(ref p) = self.internal[sq.get_num()] {
            if let Some(phantom) = to_phantom(&p) {
                return phantom.1 == Uai1 && phantom.2 == sid && self.is_tam2_hue(sq);
            }
        }
        false
    }
}

pub fn is_inherent_tam2_hue(sq: Square2) -> bool {
    let sq = sq.get_num();
    20 <= sq && sq <= 60 && (sq % 10 == 0 || sq % 8 == 0)
}

macro_rules! vec_sq {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push(Square2::from_num($x).unwrap());
            )*
            temp_vec
        }
    };
}

pub fn get_neighbors_(q: Square2) -> Vec<Square2> {
    match q.get_num() {
        0 => vec_sq![1, 9, 10],
        8 => vec_sq![7, 16, 17],
        72 => vec_sq![63, 64, 73],
        80 => vec_sq![70, 71, 79],
        a => if 1 <= a && a <= 7 {
            vec_sq![a - 1, a + 1, a + 8, a + 9, a + 10]
        } else if 73 <= a && a <= 79 {
            vec_sq![a - 1, a + 1, a - 10, a - 9, a - 8]
        } else {
            match a % 9 {
                0 => vec_sq![a + 1, a - 9, a - 8, a + 9, a + 10],
                8 => vec_sq![a - 1, a - 9, a - 10, a + 9, a + 8],
                _ => vec_sq![a - 10, a - 9, a - 8, a - 1, a + 1, a + 8, a + 9, a + 10],
            }
        },
    }
}

pub fn get_neighbors_and_self(q: Square2) -> Vec<Square2> {
    let mut v = get_neighbors_(q);
    v.push(q);
    v
}

#[derive(Debug)]
pub enum Error {
    /// The square you're moving to is already occupied.
    AlreadyOccupied(Square2),

    /// The square you're moving from is actually empty
    EmptySquare(Square2),

    /// The square you're moving to is already occupied by Tam2.
    TamCapture,

    /// You tried to drop a piece that is not in the hand.
    NoCorrespondingPieceInHand,

    /// You tried to move the opponent's piece.
    MovingOpponentPiece,

    /// You tried to capture your own piece.
    FriendlyFire,

    /// Color of the dropped piece cannot be unambiguously inferred.
    AmbiguousColor,

    /// The actual profession differs from the expectation.
    WrongProfessionSpecified {
        expected: Option<Profession>,
        specified: Option<Profession>,
    },

    /// Declares a Dat2 whose condition is not satisfied.
    FalseDeclaration,

    /// Tried to take a piece protected by Tam2HueAUai1
    Tam2HueAUai1Violation,

    /// Tried to step on an empty square
    SteppingEmptySquare(Square),

    /// Trying a movement that the profession does not allow
    ProfessionPrivilegeExceeded(Profession, Square),

    /// Trying to capture a piece by Tam2
    CaptureByTam,

    /// Trying a movement that the Tam2 cannot do
    Tam2PrivilegeExceeded {
        _from: Square,
        _thru: Option<Square>,
        _to: Square,
    },
}

/*
const SQ_LIST: [SquareRustEnum; 81] = [
    SquareRustEnum::r_sqKA,
    SquareRustEnum::r_sqLA,
    SquareRustEnum::r_sqNA,
    SquareRustEnum::r_sqTA,
    SquareRustEnum::r_sqZA,
    SquareRustEnum::r_sqXA,
    SquareRustEnum::r_sqCA,
    SquareRustEnum::r_sqMA,
    SquareRustEnum::r_sqPA,
    SquareRustEnum::r_sqKE,
    SquareRustEnum::r_sqLE,
    SquareRustEnum::r_sqNE,
    SquareRustEnum::r_sqTE,
    SquareRustEnum::r_sqZE,
    SquareRustEnum::r_sqXE,
    SquareRustEnum::r_sqCE,
    SquareRustEnum::r_sqME,
    SquareRustEnum::r_sqPE,
    SquareRustEnum::r_sqKI,
    SquareRustEnum::r_sqLI,
    SquareRustEnum::r_sqNI,
    SquareRustEnum::r_sqTI,
    SquareRustEnum::r_sqZI,
    SquareRustEnum::r_sqXI,
    SquareRustEnum::r_sqCI,
    SquareRustEnum::r_sqMI,
    SquareRustEnum::r_sqPI,
    SquareRustEnum::r_sqKU,
    SquareRustEnum::r_sqLU,
    SquareRustEnum::r_sqNU,
    SquareRustEnum::r_sqTU,
    SquareRustEnum::r_sqZU,
    SquareRustEnum::r_sqXU,
    SquareRustEnum::r_sqCU,
    SquareRustEnum::r_sqMU,
    SquareRustEnum::r_sqPU,
    SquareRustEnum::r_sqKO,
    SquareRustEnum::r_sqLO,
    SquareRustEnum::r_sqNO,
    SquareRustEnum::r_sqTO,
    SquareRustEnum::r_sqZO,
    SquareRustEnum::r_sqXO,
    SquareRustEnum::r_sqCO,
    SquareRustEnum::r_sqMO,
    SquareRustEnum::r_sqPO,
    SquareRustEnum::r_sqKY,
    SquareRustEnum::r_sqLY,
    SquareRustEnum::r_sqNY,
    SquareRustEnum::r_sqTY,
    SquareRustEnum::r_sqZY,
    SquareRustEnum::r_sqXY,
    SquareRustEnum::r_sqCY,
    SquareRustEnum::r_sqMY,
    SquareRustEnum::r_sqPY,
    SquareRustEnum::r_sqKAI,
    SquareRustEnum::r_sqLAI,
    SquareRustEnum::r_sqNAI,
    SquareRustEnum::r_sqTAI,
    SquareRustEnum::r_sqZAI,
    SquareRustEnum::r_sqXAI,
    SquareRustEnum::r_sqCAI,
    SquareRustEnum::r_sqMAI,
    SquareRustEnum::r_sqPAI,
    SquareRustEnum::r_sqKAU,
    SquareRustEnum::r_sqLAU,
    SquareRustEnum::r_sqNAU,
    SquareRustEnum::r_sqTAU,
    SquareRustEnum::r_sqZAU,
    SquareRustEnum::r_sqXAU,
    SquareRustEnum::r_sqCAU,
    SquareRustEnum::r_sqMAU,
    SquareRustEnum::r_sqPAU,
    SquareRustEnum::r_sqKIA,
    SquareRustEnum::r_sqLIA,
    SquareRustEnum::r_sqNIA,
    SquareRustEnum::r_sqTIA,
    SquareRustEnum::r_sqZIA,
    SquareRustEnum::r_sqXIA,
    SquareRustEnum::r_sqCIA,
    SquareRustEnum::r_sqMIA,
    SquareRustEnum::r_sqPIA,
];
fn int_to_SquareRustEnum(i: usize) -> SquareRustEnum {
    SQ_LIST[i]
}

pub fn Square_to_SquareRustEnum(sq: sq) -> SquareRustEnum {

}

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum SquareRustEnum {
    r_sqKA,
    r_sqLA,
    r_sqNA,
    r_sqTA,
    r_sqZA,
    r_sqXA,
    r_sqCA,
    r_sqMA,
    r_sqPA,
    r_sqKE,
    r_sqLE,
    r_sqNE,
    r_sqTE,
    r_sqZE,
    r_sqXE,
    r_sqCE,
    r_sqME,
    r_sqPE,
    r_sqKI,
    r_sqLI,
    r_sqNI,
    r_sqTI,
    r_sqZI,
    r_sqXI,
    r_sqCI,
    r_sqMI,
    r_sqPI,
    r_sqKU,
    r_sqLU,
    r_sqNU,
    r_sqTU,
    r_sqZU,
    r_sqXU,
    r_sqCU,
    r_sqMU,
    r_sqPU,
    r_sqKO,
    r_sqLO,
    r_sqNO,
    r_sqTO,
    r_sqZO,
    r_sqXO,
    r_sqCO,
    r_sqMO,
    r_sqPO,
    r_sqKY,
    r_sqLY,
    r_sqNY,
    r_sqTY,
    r_sqZY,
    r_sqXY,
    r_sqCY,
    r_sqMY,
    r_sqPY,
    r_sqKAI,
    r_sqLAI,
    r_sqNAI,
    r_sqTAI,
    r_sqZAI,
    r_sqXAI,
    r_sqCAI,
    r_sqMAI,
    r_sqPAI,
    r_sqKAU,
    r_sqLAU,
    r_sqNAU,
    r_sqTAU,
    r_sqZAU,
    r_sqXAU,
    r_sqCAU,
    r_sqMAU,
    r_sqPAU,
    r_sqKIA,
    r_sqLIA,
    r_sqNIA,
    r_sqTIA,
    r_sqZIA,
    r_sqXIA,
    r_sqCIA,
    r_sqMIA,
    r_sqPIA,
}
*/
