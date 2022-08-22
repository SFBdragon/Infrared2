pub mod game;
pub mod parse;
pub mod board;
pub mod epd;
pub mod search;
pub mod opening;
pub mod syzygy;


pub use board::Board;
pub use game::{Game, SearchHandle};
pub use search::{SearchInfo, SearchEval, time::TimeControl};

use std::{fmt::Debug, ops::Not};
use board::zobrist::PosHashMap;


/// Evaluates a boolean expression:
/// * Where `true` returns `Ok(())`
/// * Where `false` returns `Err(&'static str)` describing the failure.
/// 
/// In practice, it's useful as a recoverable `assert!`.
#[macro_export]
macro_rules! as_result {
    ($cond:expr) => {
        bool::then_some($cond, ()).ok_or(
            concat!(
                "assertion failed: `",
                stringify!($cond),
                "`"
            )
        )
    };
}



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Side {
    White = 1,
    Black = 0,
}
impl Side {
    pub fn is_white(self) -> bool {
        self == Self::White
    }
    pub fn is_black(self) -> bool {
        self == Self::Black
    }
    pub fn flip(self) -> Self {
        match self {
            Side::White => Side::Black,
            Side::Black => Side::White,
        }
    }
}
impl Not for Side {
    type Output = Side;

    fn not(self) -> Self::Output {
        self.flip()
    }
}


/// A square on a chess board.
/// Represented as a zero-based byte index into `a1, a2, ... , b1, b2, ... , h8`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Sq(u8);
impl Sq {
    pub const A1: Sq = Sq(0o00); pub const B1: Sq = Sq(0o01); 
    pub const C1: Sq = Sq(0o02); pub const D1: Sq = Sq(0o03); 
    pub const E1: Sq = Sq(0o04); pub const F1: Sq = Sq(0o05); 
    pub const G1: Sq = Sq(0o06); pub const H1: Sq = Sq(0o07); 
    pub const A2: Sq = Sq(0o10); pub const B2: Sq = Sq(0o11); 
    pub const C2: Sq = Sq(0o12); pub const D2: Sq = Sq(0o13); 
    pub const E2: Sq = Sq(0o14); pub const F2: Sq = Sq(0o15); 
    pub const G2: Sq = Sq(0o16); pub const H2: Sq = Sq(0o17); 
    pub const A3: Sq = Sq(0o20); pub const B3: Sq = Sq(0o21); 
    pub const C3: Sq = Sq(0o22); pub const D3: Sq = Sq(0o23); 
    pub const E3: Sq = Sq(0o24); pub const F3: Sq = Sq(0o25); 
    pub const G3: Sq = Sq(0o26); pub const H3: Sq = Sq(0o27); 
    pub const A4: Sq = Sq(0o30); pub const B4: Sq = Sq(0o31); 
    pub const C4: Sq = Sq(0o32); pub const D4: Sq = Sq(0o33); 
    pub const E4: Sq = Sq(0o34); pub const F4: Sq = Sq(0o35); 
    pub const G4: Sq = Sq(0o36); pub const H4: Sq = Sq(0o37); 
    pub const A5: Sq = Sq(0o40); pub const B5: Sq = Sq(0o41); 
    pub const C5: Sq = Sq(0o42); pub const D5: Sq = Sq(0o43); 
    pub const E5: Sq = Sq(0o44); pub const F5: Sq = Sq(0o45); 
    pub const G5: Sq = Sq(0o46); pub const H5: Sq = Sq(0o47); 
    pub const A6: Sq = Sq(0o50); pub const B6: Sq = Sq(0o51); 
    pub const C6: Sq = Sq(0o52); pub const D6: Sq = Sq(0o53); 
    pub const E6: Sq = Sq(0o54); pub const F6: Sq = Sq(0o55); 
    pub const G6: Sq = Sq(0o56); pub const H6: Sq = Sq(0o57); 
    pub const A7: Sq = Sq(0o60); pub const B7: Sq = Sq(0o61); 
    pub const C7: Sq = Sq(0o62); pub const D7: Sq = Sq(0o63); 
    pub const E7: Sq = Sq(0o64); pub const F7: Sq = Sq(0o65); 
    pub const G7: Sq = Sq(0o66); pub const H7: Sq = Sq(0o67); 
    pub const A8: Sq = Sq(0o70); pub const B8: Sq = Sq(0o71); 
    pub const C8: Sq = Sq(0o72); pub const D8: Sq = Sq(0o73); 
    pub const E8: Sq = Sq(0o74); pub const F8: Sq = Sq(0o75); 
    pub const G8: Sq = Sq(0o76); pub const H8: Sq = Sq(0o77);


    pub const fn new(i: u8) -> Self {
        assert!(i < 64);
        Self(i)
    }
    pub const fn file_rank(file: u8, rank: u8) -> Self {
        assert!(file < 8 && rank < 8);
        Self(file + rank * 8)
    }
    /// `Sq` from a bitmap's lowest set bit.
    pub const fn lsb(bm: u64) -> Self {
        Self(bm.trailing_zeros() as u8)
    }
    

    /// Get the underlying `u8`, returns `0..=63`.
    pub const fn u8(self) -> u8 { self.0 }
    /// Get the underlying `u8` representation as `usize`, returns `0..=63`.
    pub const fn us(self) -> usize { self.0 as usize }
    /// Get the rank, returns `0..=7`.
    pub const fn rank(self) -> u8 { self.0 / 8 }
    /// Get the file, returns `0..=7`.
    pub const fn file(self) -> u8 { self.0 % 8 }
    
    /// Get a bitmask with the corresponding bit set.
    pub const fn bm(self) -> u64 { 1u64 << self.0 }

    /// Mirror the rank.
    pub const fn flip(self) -> Self { Self(self.0 ^ 0o70) }

    /// Flips the rank if `side == Side::Black`.
    /// Converts between Infrared and standard Chess square encoding.
    pub const fn cflip(self, side: Side) -> Self {
        match side {
            Side::White => self,
            Side::Black => self.flip(),
        }
    }
}
impl Debug for Sq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Sq").field(&format_args!("{:#o}", self.0)).finish()
    }
}
impl Into<u8> for Sq {
    fn into(self) -> u8 {
        self.0
    }
}
impl TryFrom<u8> for Sq {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > 64 { return Err(()); }
        Ok(Sq::new(value))
    }
}

/// Loop through the square of each set bit from least to most significant.
#[macro_export]
macro_rules! for_sq {
    ($sq:ident in $bb:expr => $blk:expr) => {
        let mut t = $bb;
        while t != 0 {
            let $sq = Sq::lsb(t);
            t &= t - 1;
            $blk
        }
    };
}




#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub from: Sq,
    pub to: Sq,
    /// The moved piece identity, or otherwise promotion piece.
    pub piece: Piece,
}
impl Move {
    pub const KS_CASTLE: Move = Move::new(Sq::E1, Sq::G1, Piece::King);
    pub const QS_CASTLE: Move = Move::new(Sq::E1, Sq::C1, Piece::King);

    pub const fn new(from: Sq, to: Sq, piece: Piece) -> Self {
        Self { from, to, piece }
    }

    pub const fn cflip(self, side: Side) -> Move {
        Move::new(self.from.cflip(side), self.to.cflip(side), self.piece)
    }
}

pub const KING: u8 = 0;
pub const QUEEN: u8 = 1;
pub const ROOK: u8 = 2;
pub const BISHOP: u8 = 3;
pub const KNIGHT: u8 = 4;
pub const PAWN: u8 = 5;

pub const KING_IDX: usize = KING as usize;
pub const QUEEN_IDX: usize = QUEEN as usize;
pub const ROOK_IDX: usize = ROOK as usize;
pub const BISHOP_IDX: usize = BISHOP as usize;
pub const KNIGHT_IDX: usize = KNIGHT as usize;
pub const PAWN_IDX: usize = PAWN as usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Piece {
    King = KING,
    Queen = QUEEN,
    Rook = ROOK,
    Bishop = BISHOP,
    Knight = KNIGHT,
    Pawn = PAWN,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameOver {
    Checkmate,
    Stalemate,
    ThreefoldRepetition,
    FiftyMoveRule,
    InsufficientMaterial,
}



#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CastleRights(u8);
impl CastleRights {
    const KINGSIDE: u8 = 1 << 0;
    const QUEENSIDE: u8 = 1 << 1;

    pub const fn new(kingside: bool, queenside: bool) -> Self {
        Self(Self::KINGSIDE * kingside as u8 | Self::QUEENSIDE * queenside as u8)
    }

    /// Check if kingside castling is allowable.
    pub const fn kingside(self) -> bool { self.0 & Self::KINGSIDE != 0 }
    /// Check if queenside castling is allowable.
    pub const fn queenside(self) -> bool { self.0 & Self::QUEENSIDE != 0 }
    /// Check if either side castling is allowable.
    pub const fn any(self) -> bool { self.0 != 0 }
    
    /// Void the right to kingside castle.
    pub fn void_kingside(&mut self) { self.0 &= !Self::KINGSIDE; }
    /// Void the right to queenside castle.
    pub fn void_queenside(&mut self) { self.0 &= !Self::QUEENSIDE; }
}
impl Debug for CastleRights {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CastleRights")
            .field("kingside", &self.kingside())
            .field("queenside", &self.queenside())
            .finish()
    }
}

