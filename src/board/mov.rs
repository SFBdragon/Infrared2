//! Piece move generation, packing, and iteration.

use std::mem::MaybeUninit;

use crate::{
    for_sq, 
    board::{fend, Board, Piece, CastleFlags}
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

    pub fn king_moves_actv(&self, sq: u8) -> MoveSet {
        let mut to_set = fend::king_fend(sq) & !self.actv;

        // castling
        if self.actv_castle_flags.contains(CastleFlags::KINGSIDE) {
            if self.actv & 0x60 == 0 && self.can_kingside_castle() {
                to_set |= 0x40;
            }
        }
        if self.actv_castle_flags.contains(CastleFlags::QUEENSIDE) {
            if self.actv & 0xE == 0 && self.can_queenside_castle() {
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
    pub fn get_moveset_actv(&self, tab: &mut MoveSetTable) {
        for_sq!(sq in self.actv_pawns & 0x00FF000000000000 => { tab.push_prom(self.pawn_proms_actv(sq)) });
        for_sq!(sq in self.actv_pawns & !0x00FF000000000000 => { tab.push(self.pawn_moves_actv(sq)) });
        for_sq!(sq in self.actv_knights => { tab.push(self.knight_moves_actv(sq)) });
        for_sq!(sq in self.actv_bishops => { tab.push(self.bishop_moves_actv(sq)) });
        for_sq!(sq in self.actv_rooks => { tab.push(self.rook_moves_actv(sq)) });
        for_sq!(sq in self.actv_queens => { tab.push(self.queen_moves_actv(sq)) });
        tab.push(self.king_moves_actv(self.actv_king_sq));
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

/// Iterator over all promotion possiblities for a piece.
pub type PromSetIter = GenPromSetIter::<PROM_PIECE_COUNT>;
/// Iterator over engine-relevant promotion possiblities for a piece.
pub type AiPromSetIter = GenPromSetIter::<AI_PROM_PIECE_COUNT>;

const PROM_PIECE_COUNT: usize = 4;
const AI_PROM_PIECE_COUNT: usize = 2;
const PROM_PIECE_SET: [Piece; 4] = [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop];

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


/// Represents a collection of pseudo-legal moves:
/// * Normal moves packed and labelled by piece.
/// * Promotions packed by pawn.
#[derive(Debug, Clone)]
pub struct MoveSetTable {
    len: usize, 
    sets: [MaybeUninit<MoveSet>; 16],

    prom_len: usize,
    prom_sets: [MaybeUninit<PromSet>; 8],
}

impl MoveSetTable {
    pub fn new() -> Self {
        Self {
            len: 0,
            sets: [MaybeUninit::uninit(); 16],
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
            assert!(self.prom_len < 16);
            self.sets[self.len] = MaybeUninit::new(move_set);
            self.len += 1;
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
            &*(&self.sets[..self.len] as *const [_] as *const [_])
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
