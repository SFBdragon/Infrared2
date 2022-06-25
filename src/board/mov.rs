//! Piece move generation, packing, and iteration.

use std::mem::MaybeUninit;

use crate::{
    for_msk, for_sq, 
    board::{fend, Board, Piece, CastleFlags}
};

impl Board {
    #[inline]
    pub fn get_pawn_moves_actv(&self, from: u64) -> MoveSet {
        debug_assert!(from & 0xFFFF0000000000FF == 0);
        let mut to_set = 0u64;

        to_set |= from << 0o10 & !self.all;
        to_set |= to_set & 0xFF0000 << 0o10 & !self.all;

        let capt = self.idle | self.en_passant;
        to_set |= (from & !0x0101010101010101) << 0o7  & capt;
        to_set |= (from & !0x8080808080808080) << 0o11 & capt;
    
        MoveSet { from, to_set, piece: Piece::Pawn }
    }

    #[inline]
    pub fn get_pawn_proms_actv(&self, from: u64) -> PromSet {
        debug_assert!(from & !0x00FF000000000000 == 0);
        let mut to_set = 0u64;

        to_set |= from << 0o10 & !self.all;

        to_set |= (from & !0x0101010101010101) << 0o7  & self.idle;
        to_set |= (from & !0x8080808080808080) << 0o11 & self.idle;
    
        PromSet { from, to_set }
    }

    #[inline]
    pub fn get_knight_moves_actv(&self, sq: usize) -> MoveSet {
        MoveSet {
            from: 1 << sq,
            to_set: fend::get_knight_fend(sq) & !self.actv,
            piece: Piece::Knight,
        }
    }

    #[inline]
    pub fn get_bishop_moves_actv(&self, sq: usize) -> MoveSet {
        MoveSet {
            from: 1 << sq,
            to_set: fend::get_bishop_fend(sq, self.all) & !self.actv,
            piece: Piece::Bishop,
        }
    }

    #[inline]
    pub fn get_rook_moves_actv(&self, sq: usize) -> MoveSet {
        MoveSet {
            from: 1 << sq,
            to_set: fend::get_rook_fend(sq, self.all) & !self.actv,
            piece: Piece::Rook,
        }
    }

    #[inline]
    pub fn get_queen_moves_actv(&self, sq: usize) -> MoveSet {
        MoveSet {
            from: 1 << sq,
            to_set: fend::get_queen_fend(sq, self.all) & !self.actv,
            piece: Piece::Queen,
        }
    }

    #[inline]
    pub fn get_king_moves_actv(&self, sq: usize) -> MoveSet {
        let mut to_set = fend::get_king_fend(sq) & !self.actv & !self.idle_fend;

        // castling
        if self.actv_castle_flags.contains(CastleFlags::KINGSIDE) {
            if self.actv & 0x60 == 0 && self.idle_fend & 0x70 == 0 {
                to_set |= 0x40;
            }
        }
        if self.actv_castle_flags.contains(CastleFlags::QUEENSIDE) {
            if self.actv & 0xE == 0 && self.idle_fend & 0x1C == 0 {
                to_set |= 0x4;
            }
        }
        
        MoveSet {
            from: 1 << sq,
            to_set, piece:
            Piece::King
        }
    }


    /// Provides illegal moves wrt checking.
    pub fn get_moveset_actv(&self, tab: &mut MoveSetTable) {
        for_msk!(from in self.pawns & self.actv & 0x00FF000000000000 => {
            tab.push_prom(self.get_pawn_proms_actv(from));
        });
        for_msk!(from in self.pawns & self.actv & !0x00FF000000000000 => {
            tab.push(self.get_pawn_moves_actv(from));
        });
        for_sq!(sq in self.knights & self.actv => {
            tab.push(self.get_knight_moves_actv(sq));
        });
        for_sq!(sq in self.bishops & self.actv => {
            tab.push(self.get_bishop_moves_actv(sq));
        });
        for_sq!(sq in self.rooks & self.actv => {
            tab.push(self.get_rook_moves_actv(sq));
        });
        for_sq!(sq in self.queens & self.actv => {
            tab.push(self.get_queen_moves_actv(sq));
        });
        let king_sq = (self.kings & self.actv).trailing_zeros() as usize;
        tab.push(self.get_king_moves_actv(king_sq));
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveSet {
    pub from: u64,
    pub to_set: u64,
    pub piece: Piece,
}

impl MoveSet {
    #[inline]
    pub fn iter(&self) -> MoveSetIter {
        MoveSetIter {
            from: self.from,
            to_set: self.to_set,
            piece: self.piece,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PromSet {
    pub from: u64,
    pub to_set: u64,
}

impl PromSet {
    #[inline]
    pub fn iter(&self) -> PromSetIter {
        PromSetIter {
            from: self.from,
            to_set: self.to_set,
            to: 0,
            piece_index: PROM_PIECE_COUNT
        }
    }

    #[inline]
    pub fn ai_iter(&self) -> AiPromSetIter {
        AiPromSetIter {
            from: self.from,
            to_set: self.to_set,
            to: 0,
            piece_index: AI_PROM_PIECE_COUNT
        }
    }
}


#[derive(Debug, Clone)]
pub struct MoveSetIter {
    from: u64,
    to_set: u64,
    piece: Piece,
}

impl Iterator for MoveSetIter {
    type Item = (u64, u64, Piece);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.to_set == 0 { return None; }

        let tss1 = self.to_set - 1;
        let to = self.to_set & !tss1;
        self.to_set &= tss1;

        Some((self.from, to, self.piece))
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
    from: u64,
    to_set: u64,
    to: u64, 
    piece_index: usize,
}

impl<const PIECE_COUNT: usize> Iterator for GenPromSetIter<PIECE_COUNT> {
    type Item = (u64, u64, Piece);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.piece_index == PIECE_COUNT {
            if self.to_set == 0 {
                return None;
            }

            self.piece_index = 0;

            let tss1 = self.to_set - 1;
            self.to = self.to_set & !tss1;
            self.to_set &= tss1;
        }
        
        let prom = PROM_PIECE_SET[self.piece_index];
        self.piece_index += 1;
        Some((self.from, self.to, prom))
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
