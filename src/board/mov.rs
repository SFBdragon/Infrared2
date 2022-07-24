//! Piece move generation.

use crate::{
    for_sq, Board, Move, Piece,
    board::{fend, CastleRights}
};


pub const PAWN_PROM_RANK: u64 = 0x00FF000000000000;

impl Board {
    #[inline]
    fn pawn_moves(&self, sq: u8) -> MoveSet {
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
    fn pawn_proms(&self, sq: u8) -> PromSet {
        let mut to_set = 0u64;
        let from = 1u64 << sq;
        debug_assert!(from & !PAWN_PROM_RANK == 0);

        to_set |= from << 0o10 & !self.all;

        to_set |= (from & !0x0101010101010101) << 0o7  & self.idle;
        to_set |= (from & !0x8080808080808080) << 0o11 & self.idle;
    
        PromSet { to_set, from_sq: sq }
    }

    #[inline]
    fn knight_moves(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::knight_fend(sq) & !self.actv,
            piece: Piece::Knight,
        }
    }

    #[inline]
    fn bishop_moves(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::bishop_fend(sq, self.all) & !self.actv,
            piece: Piece::Bishop,
        }
    }

    #[inline]
    fn rook_moves(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::rook_fend(sq, self.all) & !self.actv,
            piece: Piece::Rook,
        }
    }

    #[inline]
    fn queen_moves(&self, sq: u8) -> MoveSet {
        MoveSet {
            from_sq: sq,
            to_set: fend::queen_fend(sq, self.all) & !self.actv,
            piece: Piece::Queen,
        }
    }

    /// Generates legal king moves only.
    fn king_moves(&self) -> MoveSet {
        // calculate all covered squares by idle (except king-covered)
        let mut idle_fend = 0;
        idle_fend |= fend::king_fend(self.idle_king_sq); 
        idle_fend |= fend::pawns_fend_idle(self.pawns & self.idle);
        for_sq!(sq in self.knights & self.idle => idle_fend |= fend::knight_fend(sq));
        let all = self.all & !(1 << self.actv_king_sq);
        for_sq!(sq in (self.bishops | self.queens) & self.idle => idle_fend |= fend::bishop_fend(sq, all));
        for_sq!(sq in (self.rooks   | self.queens) & self.idle => idle_fend |= fend::rook_fend(sq, all));

        // normal king moves
        let mut to = fend::king_fend(self.actv_king_sq) & !self.actv & !idle_fend;

        // castling
        if self.actv_castle_rights.contains(CastleRights::KINGSIDE) {
            if self.all & 0x60 == 0 && idle_fend & 0x70 != 0 { to |= 0x40; }
        }
        if self.actv_castle_rights.contains(CastleRights::QUEENSIDE) {
            if self.all &  0xE == 0 && idle_fend & 0x1C != 0 { to |= 0x4; }
        }
        
        MoveSet { from_sq: self.actv_king_sq, to_set: to, piece: Piece::King }
    }

    /// Generate all legal moves until closure returns true.
    #[inline]
    pub fn for_mov<F: FnMut(Move) -> bool>(&self, mut f: F) {
        // while seemingly ripe for refactor, note: return embedment; highly layered closures tank search perf; column associativity -> maintainability
        for_sq!(sq in self.pawns & self.actv &  PAWN_PROM_RANK => for m in self.pawn_proms  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.pawns & self.actv & !PAWN_PROM_RANK => for m in self.pawn_moves  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.knights & self.actv                 => for m in self.knight_moves(sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.bishops & self.actv                 => for m in self.bishop_moves(sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.rooks & self.actv                   => for m in self.rook_moves  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.queens & self.actv                  => for m in self.queen_moves (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        /* programming is an art! */                              for m in self.king_moves  (  ).iter() { if                     (&mut f)(m) { return } }
    }
    
    /// Checks whether a generates pseudo-legal move is legal.
    pub fn is_legal(&self, Move { from_sq, to_sq, piece }: Move) -> bool {
        let king_sq = if let Piece::King = piece { to_sq } else { self.actv_king_sq };
        let (from, to) = (1 << from_sq, 1 << to_sq);

        let ep = if to & self.en_passant != 0 && piece == Piece::Pawn { to >> 0o10 } else { 0 }; // en passant
        let all = self.all & !from & !ep | to;
        let idle = self.idle & !to & !ep;
        
        fend::knight_fend(king_sq) & self.knights & idle == 0
        && fend::bishop_fend(king_sq, all) & (self.bishops | self.queens) & idle == 0
        && fend::rook_fend(king_sq, all) & (self.rooks | self.queens) & idle == 0
        && fend::pawn_fend_actv(king_sq) & self.pawns & idle == 0
    }

    /// Test if an arbitrary move is valid and legal in this position.
    pub fn is_valid(&self, mov: Move) -> bool {
        let piece = mov.piece;
        let from_sq = mov.from_sq;
        let from = 1u64 << mov.from_sq;
        let to = 1u64 << mov.to_sq;

        // check if an actv piece exists on this square
        if from & self.actv == 0 { return false; }

        // handle promotion, else handle normal move
        if piece != Piece::Pawn && from & self.pawns != 0 && from & PAWN_PROM_RANK != 0 {
            // ensure valid promotion piece
            if piece == Piece::Pawn || piece == Piece::King { return false; }

            // ensure valid move
            self.pawn_proms(from_sq).to_set & to != 0 && self.is_legal(mov)
        } else {
            // ensure piece identity
            if self.get_piece_at(from) != Some(piece) { return false; }

            // ensure valid move
            let to_set = match piece {
                Piece::King => self.king_moves().to_set,
                Piece::Queen => self.queen_moves(from_sq).to_set,
                Piece::Rook => self.rook_moves(from_sq).to_set,
                Piece::Bishop => self.bishop_moves(from_sq).to_set,
                Piece::Knight => self.knight_moves(from_sq).to_set,
                Piece::Pawn => self.pawn_moves(from_sq).to_set,
            };

            to_set & to != 0 && self.is_legal(mov)
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
            piece_index: 0,
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

/// Iterator over the extrapolated promotion possiblities from a `PromSet`.
#[derive(Debug, Clone)]
pub struct PromSetIter {
    to_set: u64,
    from_sq: u8,
    to_sq: u8, 
    piece_index: usize,
}

impl Iterator for PromSetIter {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        const PROM_PIECES: [Piece; 4] = [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop];

        if self.piece_index == PROM_PIECES.len() {
            if self.to_set == 0 {
                return None;
            }

            self.piece_index = 0;

            self.to_sq = self.to_set.trailing_zeros() as u8;
            self.to_set &= self.to_set - 1;
        }
        
        let prom = PROM_PIECES[self.piece_index];
        self.piece_index += 1;
        Some(Move::new(self.from_sq, self.to_sq, prom))
    }
}
