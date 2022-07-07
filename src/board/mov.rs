//! Piece move generation, packing, and iteration.

use std::mem::MaybeUninit;

use crate::{
    for_sq, 
    board::{fend, Board, Piece, CastleRights}
};

use super::Move;

impl Board {
    #[inline]
    pub fn pawn_moves_actv(&self, sq: u8) -> MoveSet {
        let mut to_set = 0u64;
        let from = 1u64 << sq;
        debug_assert!(from & 0xFFFF0000000000FF == 0);

        to_set |= from << 0o10 & !self.all;
        to_set |= (to_set & 0xFF0000) << 0o10 & !self.all;

        let capt = self.idle | self.en_passant;
        to_set |= (from & !0x0101010101010101) << 0o7  & capt;
        to_set |= (from & !0x8080808080808080) << 0o11 & capt;
    
        MoveSet { to_set, from_sq: sq, piece: Piece::Pawn }
    }

    #[inline]
    pub fn pawn_proms_actv(&self, sq: u8) -> PromSet {
        let mut to_set = 0u64;
        let from = 1u64 << sq;
        debug_assert!(from & !0x00FF000000000000 == 0);

        to_set |= from << 0o10 & !self.all;

        to_set |= (from & !0x0101010101010101) << 0o7  & self.idle;
        to_set |= (from & !0x8080808080808080) << 0o11 & self.idle;
    
        PromSet { to_set, from_sq: sq }
    }

    #[inline]
    pub fn knight_moves_actv(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::knight_fend(sq) & !self.actv,
            piece: Piece::Knight,
        }
    }

    #[inline]
    pub fn bishop_moves_actv(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::bishop_fend(sq, self.all) & !self.actv,
            piece: Piece::Bishop,
        }
    }

    #[inline]
    pub fn rook_moves_actv(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::rook_fend(sq, self.all) & !self.actv,
            piece: Piece::Rook,
        }
    }

    #[inline]
    pub fn queen_moves_actv(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::queen_fend(sq, self.all) & !self.actv,
            piece: Piece::Queen,
        }
    }

    /// Check whether the `actv` king would be kingside castling through check.
    fn can_kingside_castle(&self) -> bool {
        let idle_strt = (self.rooks | self.queens) & self.idle;
        let idle_diag = (self.bishops | self.queens) & self.idle;
        0xF8DC00 & self.idle & self.knights == 0
        && fend::bishop_fend(4, self.all) & idle_diag == 0
        && fend::bishop_fend(5, self.all) & idle_diag == 0
        && fend::bishop_fend(6, self.all) & idle_diag == 0
        && fend::rook_fend(4, self.all) & idle_strt == 0
        && fend::rook_fend(5, self.all) & idle_strt == 0
        && fend::rook_fend(6, self.all) & idle_strt == 0
        && 0xF800 & self.idle & self.pawns == 0
        && 0xF8F8 & (1u64 << self.idle_king_sq) == 0
    }
    /// Check whether the `actv` king would be queenside castling through check.
    fn can_queenside_castle(&self) -> bool {
        let idle_strt = (self.rooks | self.queens) & self.idle;
        let idle_diag = (self.bishops | self.queens) & self.idle;
        0x3E7700 & self.idle & self.knights == 0
        && fend::bishop_fend(4, self.all) & idle_diag == 0
        && fend::bishop_fend(3, self.all) & idle_diag == 0
        && fend::bishop_fend(2, self.all) & idle_diag == 0
        && fend::rook_fend(4, self.all) & idle_strt == 0
        && fend::rook_fend(3, self.all) & idle_strt == 0
        && fend::rook_fend(2, self.all) & idle_strt == 0
        && 0x3E00 & self.idle & self.pawns == 0
        && 0x3E3E & (1u64 << self.idle_king_sq) == 0
    }
    
    pub fn king_moves_actv(&self, sq: u8) -> MoveSet {
        let mut to_set = fend::king_fend(sq) & !self.actv;

        // castling
        if self.actv_castle_rights.contains(CastleRights::KINGSIDE) {
            if self.all & 0x60 == 0 && self.can_kingside_castle() {
                to_set |= 0x40;
            }
        }
        if self.actv_castle_rights.contains(CastleRights::QUEENSIDE) {
            if self.all & 0xE == 0 && self.can_queenside_castle() {
                to_set |= 0x4;
            }
        }
        
        MoveSet {
            from_sq: sq,
            to_set,
            piece: Piece::King,
        }
    }


    /// Provides illegal moves wrt checking.
    pub fn get_move_tab_actv(&self, tab: &mut MoveSetTable) {
        for_sq!(sq in self.pawns & self.actv & 0x00FF000000000000 => {
            tab.push_prom(self.pawn_proms_actv(sq));
        });
        for_sq!(sq in self.pawns & self.actv & !0x00FF000000000000 => {
            tab.push(self.pawn_moves_actv(sq));
        });
        for_sq!(sq in self.knights & self.actv => { tab.push(self.knight_moves_actv(sq)); });
        for_sq!(sq in self.bishops & self.actv => { tab.push(self.bishop_moves_actv(sq)); });
        for_sq!(sq in self.rooks & self.actv => { tab.push(self.rook_moves_actv(sq)); });
        for_sq!(sq in self.queens & self.actv => { tab.push(self.queen_moves_actv(sq)); });
        tab.push(self.king_moves_actv(self.actv_king_sq));
    }
    
    pub fn get_role_move_tab_actv(&self, piece: Piece, tab: &mut MoveSetTable) {
        match piece {
            Piece::Pawn => {
                for_sq!(sq in self.pawns & self.actv & 0x00FF000000000000 => {
                    tab.push_prom(self.pawn_proms_actv(sq));
                });
                for_sq!(sq in self.pawns & self.actv & !0x00FF000000000000 => {
                    tab.push(self.pawn_moves_actv(sq));
                });
            },
            Piece::Knight => {
                for_sq!(sq in self.knights & self.actv => {
                    tab.push(self.knight_moves_actv(sq));
                });
            },
            Piece::Bishop => {
                for_sq!(sq in self.bishops & self.actv => {
                    tab.push(self.bishop_moves_actv(sq)); });
            },
            Piece::Rook => {
                for_sq!(sq in self.rooks & self.actv => { 
                    tab.push(self.rook_moves_actv(sq)); 
                });
            },
            Piece::Queen => {
                for_sq!(sq in self.queens & self.actv => { 
                    tab.push(self.queen_moves_actv(sq)); 
                });
            },
            Piece::King => tab.push(self.king_moves_actv(self.actv_king_sq)),
        }
    }

    pub fn get_move_set_actv(&self, from_sq: u8, piece: Piece) -> Set {
        let from = 1u64 << from_sq;
        if piece == Piece::Pawn && from & 0x00FF000000000000 != 0 {
            Set::Prom(self.pawn_proms_actv(from_sq))
        } else {
            Set::Move(match piece {
                Piece::King => self.king_moves_actv(from_sq),
                Piece::Queen => self.queen_moves_actv(from_sq),
                Piece::Rook => self.rook_moves_actv(from_sq),
                Piece::Bishop => self.bishop_moves_actv(from_sq),
                Piece::Knight => self.knight_moves_actv(from_sq),
                Piece::Pawn => self.pawn_moves_actv(from_sq),
            })
        }
    }

    /// Test if a move is a valid pseudo-legal move in the position `self`.
    /// (Generated moves will always be valid.)
    pub fn is_valid_move(&self, Move { from_sq, to_sq, piece }: Move) -> bool {
        let from = 1u64 << from_sq;
        let to = 1u64 << to_sq;

        // check if an actv piece exists on this square
        if from & self.actv == 0 { return false; }

        // handle promotion, else handle normal move
        if piece != Piece::Pawn && from & self.pawns != 0 && from & 0xFF000000000000 != 0 {
            // ensure valid promotion piece
            if piece == Piece::Pawn || piece == Piece::King { return false; }

            // ensure valid move
            self.pawn_proms_actv(from_sq).to_set & to != 0
        } else {
            // ensure piece identity
            if self.get_piece_at(from) != Some(piece) { return false; }

            // ensure valid move
            let to_set = match piece {
                Piece::King => self.king_moves_actv(from_sq).to_set,
                Piece::Queen => self.queen_moves_actv(from_sq).to_set,
                Piece::Rook => self.rook_moves_actv(from_sq).to_set,
                Piece::Bishop => self.bishop_moves_actv(from_sq).to_set,
                Piece::Knight => self.knight_moves_actv(from_sq).to_set,
                Piece::Pawn => self.pawn_moves_actv(from_sq).to_set,
            };
            to_set & to != 0
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveSet {
    pub to_set: u64,
    pub from_sq: u8,
    pub piece: Piece,
}

impl MoveSet {
    #[inline]
    pub fn iter(&self) -> MoveSetIter {
        MoveSetIter {
            from_sq: self.from_sq,
            to_set: self.to_set,
            piece: self.piece,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PromSet {
    pub to_set: u64,
    pub from_sq: u8,
}

impl PromSet {
    #[inline]
    pub fn iter(&self) -> PromSetIter {
        PromSetIter {
            from_sq: self.from_sq,
            to_set: self.to_set,
            to_sq: 0,
            piece_index: PROM_PIECE_COUNT
        }
    }

    #[inline]
    pub fn ai_iter(&self) -> AiPromSetIter {
        AiPromSetIter {
            from_sq: self.from_sq,
            to_set: self.to_set,
            to_sq: 0,
            piece_index: AI_PROM_PIECE_COUNT
        }
    }
}


#[derive(Debug, Clone)]
pub struct MoveSetIter {
    to_set: u64,
    from_sq: u8,
    piece: Piece,
}

impl Iterator for MoveSetIter {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.to_set == 0 { return None; }

        let to_sq = self.to_set.trailing_zeros() as u8;
        self.to_set &= self.to_set - 1;

        Some(Move::new(self.from_sq, to_sq, self.piece))
    }
}

const PROM_PIECE_COUNT: usize = 4;
const AI_PROM_PIECE_COUNT: usize = 2;
const PROM_PIECE_SET: [Piece; 4] = [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop];

/// Iterator over all promotion possiblities for a piece.
pub type PromSetIter = GenPromSetIter::<PROM_PIECE_COUNT>;
/// Iterator over engine-relevant promotion possiblities for a piece.
pub type AiPromSetIter = GenPromSetIter::<AI_PROM_PIECE_COUNT>;

/// Generic iterator over the extrapolated promotion possiblities
/// from a packed `PromSet` structure.
#[derive(Debug, Clone)]
pub struct GenPromSetIter<const PIECE_COUNT: usize> {
    to_set: u64,
    from_sq: u8,
    to_sq: u8, 
    piece_index: usize,
}

impl<const PIECE_COUNT: usize> Iterator for GenPromSetIter<PIECE_COUNT> {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.piece_index == PIECE_COUNT {
            if self.to_set == 0 {
                return None;
            }

            self.piece_index = 0;

            self.to_sq = self.to_set.trailing_zeros() as u8;
            self.to_set &= self.to_set - 1;
        }
        
        let prom = PROM_PIECE_SET[self.piece_index];
        self.piece_index += 1;
        Some(Move::new(self.from_sq, self.to_sq, prom))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Set {
    Move(MoveSet),
    Prom(PromSet),
}

impl Set {
    pub fn get_move_sets(self) -> Option<MoveSet> {
        match self {
            Set::Move(mov) => Some(mov),
            Set::Prom(_) => None,
        }
    }
    pub fn get_prom_sets(self) -> Option<PromSet> {
        match self {
            Set::Move(_) => None,
            Set::Prom(prom) => Some(prom),
        }
    }
}

/// Represents a collection of pseudo-legal moves:
/// * Normal moves packed and labelled by piece.
/// * Promotions packed by pawn.
#[derive(Clone)]
pub struct MoveSetTable {
    move_len: usize, 
    move_sets: [MaybeUninit<MoveSet>; 16],

    prom_len: usize,
    prom_sets: [MaybeUninit<PromSet>; 8],
}

impl MoveSetTable {
    pub fn new() -> Self {
        Self {
            move_len: 0,
            move_sets: [MaybeUninit::uninit(); 16],
            prom_len: 0,
            prom_sets: [MaybeUninit::uninit(); 8],
        }
    }

    /// Append a piece's moveset to the end of the collection, if it isn't empty.
    /// ### Panics:
    /// Panics if greater than 16 sets are to be inserted.
    #[inline]
    pub fn push(&mut self, move_set: MoveSet) {
        // don't bother inserting empty movesets
        if move_set.to_set != 0 {
            assert!(self.move_len < 16);
            self.move_sets[self.move_len] = MaybeUninit::new(move_set);
            self.move_len += 1;
        }
    }
    /// Append a pawn's promotion set to the end of the collection, if it isn't empty.
    /// ### Panics:
    /// Panics if greater than 8 sets are to be inserted.
    #[inline]
    pub fn push_prom(&mut self, prom_set: PromSet) {
        // don't bother inserting empty movesets
        if prom_set.to_set != 0 {
            assert!(self.prom_len < 8);
            self.prom_sets[self.prom_len] = MaybeUninit::new(prom_set);
            self.prom_len += 1;
        }
    }


    /// Get a slice over all move sets.
    #[inline]
    pub fn get_move_sets(&self) -> &[MoveSet] {
        unsafe {
            // SAFETY: self.sets' elements up to self.len are initialized
            &*(&self.move_sets[..self.move_len] as *const [_] as *const [_])
            // fixme: use MaybeUninit::assume_slice_init method once stable?
        }
    }
    /// Get a slice over promoting sets.
    #[inline]
    pub fn get_prom_sets(&self) -> &[PromSet] {
        unsafe {
            // SAFETY: self.prom_sets' elements up to self.prom_len are initialized
            &*(&self.prom_sets[..self.prom_len] as *const [_] as *const [_])
            // fixme: use MaybeUninit::assume_slice_init method once stable?
        }
    }
}

impl std::fmt::Debug for MoveSetTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MoveSetTable")
        .field("move_len", &self.move_len)
        .field("move_sets", &self.get_move_sets())
        .field("prom_len", &self.prom_len)
        .field("prom_sets", &self.get_prom_sets())
        .finish()
    }
}

/// Variants:
/// - `for_mov!($mov in $table => { });` - MoveSetTable iteration.
/// - `for_mov!($mov in ai $table => { });` - exclude redundant promotions.
/// - `for_mov!($mov in sq $set => { });` - Set iteration.
/// - All: optionally takes a label, e.g. 
/// `for_mov!($mov in $table until $'label => { ... break $'label; });`
/// 
/// Additionally of note, the macro is implemented with a two-level loop
/// meaning that `continue` from the label, and `break` without it,
/// will likely lead to unexpected behaviour.
#[macro_export]
macro_rules! for_mov {
    ($mov:ident in $move_table:ident $(until $brk:lifetime)? => $blk:block) => {
        $($brk:)? for prom_set in $move_table.get_prom_sets() {
            for $mov in prom_set.iter() {
                $blk
            }
        }
        $($brk:)? for move_set in $move_table.get_move_sets() {
            for $mov in move_set.iter() {
                $blk
            }
        }
    };
    ($mov:ident in ai $move_table:ident $(until $brk:lifetime)? => $blk:block) => {
        $($brk:)? for prom_set in $move_table.get_prom_sets() {
            for $mov in prom_set.ai_iter() {
                $blk
            }
        }
        $($brk:)? for move_set in $move_table.get_move_sets() {
            for $mov in move_set.iter() {
                $blk
            }
        }
    };
    ($mov:ident in sq $set:ident $(until $brk:lifetime)? => $blk:block) => {
        match self {
            Set::Move(move_set) => {
                $($brk:)? for $mov in move_set.iter() {
                    $blk
                }
            },
            Set::Prom(prom_set) => {
                $($brk:)? for $mov in prom_set.iter() {
                    $blk
                }
            },
        }
    };
}
