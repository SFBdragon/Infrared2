//! Piece move generation.

use crate::{for_sq, Sq, Board, Move, Piece, board::fend, search::ord::MoveBuffer};


pub const PAWN_PROM_RANK: u64 = 0x00FF000000000000;

impl Board {
    /// Bulk generates capturing pseudo-legal pawn moves, not en passant.
    pub fn pawn_caps(&self, buf: &mut MoveBuffer) {
        let pawns = self.pawns & self.actv & !PAWN_PROM_RANK;

        let take_right = (self.idle & !0x0101010101010101) >> 0o11 & pawns;
        for_sq!(from in take_right => buf.push(Move::offset(from, 0o11, Piece::Pawn)));
        let take_left = (self.idle & !0x8080808080808080) >> 0o7 & pawns;
        for_sq!(from in take_left => buf.push(Move::offset(from, 0o7, Piece::Pawn)));
    }

    /// Bulk generates non-capturing pseudo-legal pawn moves and en passant.
    #[inline]
    pub fn pawn_ncaps(&self, buf: &mut MoveBuffer) {
        let pawns = self.pawns & self.actv & !PAWN_PROM_RANK;

        // en passant
        if self.en_passant != 0 {
            if (self.en_passant & !0x8080808080808080) >> 0o7  & pawns != 0 {
                let from = Sq::new((self.en_passant >> 0o7  & pawns).trailing_zeros() as u8);
                buf.push(Move::offset(from, 0o7, Piece::Pawn));
            }
            if (self.en_passant & !0x0101010101010101) >> 0o11 & pawns != 0 {
                let from = Sq::new((self.en_passant >> 0o11 & pawns).trailing_zeros() as u8);
                buf.push(Move::offset(from, 0o11, Piece::Pawn));
            }
        }

        // normal advances
        let bumps = pawns & !(self.all >> 0o10);
        for_sq!(from in bumps => buf.push(Move::offset(from, 0o10, Piece::Pawn)));

        // double advances
        let jumps = bumps & 0xFF00 & !(self.all >> 0o20);
        for_sq!(from in jumps => buf.push(Move::offset(from, 0o20, Piece::Pawn)));
    }

    /// Generates pseudo-legal promotions.
    #[inline]
    fn pawn_proms(&self, sq: Sq) -> PromSet {
        let mut to_set = 0u64;
        let from_bm = sq.bm();
        debug_assert!(from_bm & !PAWN_PROM_RANK == 0);

        to_set |= from_bm << 0o10 & !self.all;

        to_set |= (from_bm & !0x0101010101010101) << 0o7  & self.idle;
        to_set |= (from_bm & !0x8080808080808080) << 0o11 & self.idle;
    
        PromSet { to_set, from: sq }
    }

    /// Generates pseudo-legal pawn moves.
    #[inline]
    fn pawn_moves(&self, sq: Sq) -> MoveSet {
        let mut to_set = 0u64;
        let from_bm = sq.bm();
        debug_assert!(from_bm & 0xFFFF0000000000FF == 0);

        to_set |= from_bm << 0o10 & !self.all;
        to_set |= (to_set & 0xFF0000) << 0o10 & !self.all;

        let capt = self.idle | self.en_passant;
        to_set |= (from_bm & !0x0101010101010101) << 0o7  & capt;
        to_set |= (from_bm & !0x8080808080808080) << 0o11 & capt;
    
        MoveSet { to_set, from: sq, piece: Piece::Pawn }
    }

    /// Generates pseudo-legal knight moves.
    #[inline]
    fn knight_moves(&self, sq: Sq, to_mask: u64) -> MoveSet {
        MoveSet {
            from: sq,
            to_set: fend::knight_fend(sq) & to_mask,
            piece: Piece::Knight,
        }
    }

    /// Generates pseudo-legal bishop moves.
    #[inline]
    fn bishop_moves(&self, sq: Sq, to_mask: u64) -> MoveSet {
        MoveSet {
            from: sq,
            to_set: fend::bishop_fend(sq, self.all) & to_mask,
            piece: Piece::Bishop,
        }
    }

    /// Generates pseudo-legal rook moves.
    #[inline]
    fn rook_moves(&self, sq: Sq, to_mask: u64) -> MoveSet {
        MoveSet {
            from: sq,
            to_set: fend::rook_fend(sq, self.all) & to_mask,
            piece: Piece::Rook,
        }
    }

    /// Generates pseudo-legal queen moves.
    #[inline]
    fn queen_moves(&self, sq: Sq, to_mask: u64) -> MoveSet {
        MoveSet {
            from: sq,
            to_set: fend::queen_fend(sq, self.all) & to_mask,
            piece: Piece::Queen,
        }
    }

    /// Generates legal king moves only.
    fn king_moves(&self) -> MoveSet {
        // calculate all covered squares by idle
        let mut idle_fend = 0;
        idle_fend |= fend::king_fend(self.idle_king); 
        idle_fend |= fend::pawns_fend_idle(self.pawns & self.idle);
        for_sq!(sq in self.knights & self.idle => idle_fend |= fend::knight_fend(sq));
        let all = self.all & !self.actv_king.bm();
        for_sq!(sq in (self.bishops | self.queens) & self.idle => idle_fend |= fend::bishop_fend(sq, all));
        for_sq!(sq in (self.rooks   | self.queens) & self.idle => idle_fend |= fend::rook_fend(sq, all));

        // normal king moves
        let mut to_set = fend::king_fend(self.actv_king) & !self.actv & !idle_fend;

        // castling
        if self.actv_castle_rights.kingside() {
            if self.all & 0x60 == 0 && idle_fend & 0x70 == 0 { to_set |= 0x40; }
        }
        if self.actv_castle_rights.queenside() {
            if self.all &  0xE == 0 && idle_fend & 0x1C == 0 { to_set |= 0x4; }
        }
        
        MoveSet { from: self.actv_king, to_set, piece: Piece::King }
    }
    
    /// Checks whether a generated pseudo-legal move is legal.
    /// 
    /// Note: never rejects king moves. Illegal king moves are never generated.
    pub fn is_legal(&self, Move { from, to, piece }: Move) -> bool {
        if let Piece::King = piece { return true; };

        let to_bm = to.bm();
        let ep = if to_bm & self.en_passant != 0 && piece == Piece::Pawn { to_bm >> 0o10 } else { 0 }; // en passant
        let all = self.all & !from.bm() & !ep | to_bm;
        let idle = self.idle & !to_bm & !ep;
        
        fend::knight_fend(self.actv_king) & self.knights & idle == 0
        && fend::bishop_fend(self.actv_king, all) & (self.bishops | self.queens) & idle == 0
        && fend::rook_fend(self.actv_king, all) & (self.rooks | self.queens) & idle == 0
        && fend::pawn_fend_actv(self.actv_king) & self.pawns & idle == 0
    }

    /// Test if an arbitrary move is valid and legal in this position.
    pub fn is_valid(&self, mv: Move) -> bool {
        let piece = mv.piece;
        let from_bm = mv.from.bm();
        let to_bm = mv.to.bm();

        // check if an actv piece exists on this square
        if from_bm & self.actv == 0 { return false; }

        // handle promotion, else handle normal move
        if piece != Piece::Pawn && from_bm & self.pawns != 0 && from_bm & PAWN_PROM_RANK != 0 {
            // ensure valid promotion piece
            if piece == Piece::Pawn || piece == Piece::King { return false; }

            // ensure valid move
            self.pawn_proms(mv.from).to_set & to_bm != 0 && self.is_legal(mv)
        } else {
            // ensure piece identity
            if self.get_piece_at(mv.from) != Some(piece) { return false; }

            // ensure valid move
            let to_set = match piece {
                Piece::King =>   self.king_moves().to_set,
                Piece::Queen =>  self.queen_moves (mv.from, !self.actv).to_set,
                Piece::Rook =>   self.rook_moves  (mv.from, !self.actv).to_set,
                Piece::Bishop => self.bishop_moves(mv.from, !self.actv).to_set,
                Piece::Knight => self.knight_moves(mv.from, !self.actv).to_set,
                Piece::Pawn =>   self.pawn_moves  (mv.from).to_set,
            };

            to_set & to_bm != 0 && self.is_legal(mv)
        }
    }

    pub fn gen_proms(&self, buf: &mut MoveBuffer) {
        for_sq!(sq in self.pawns & self.actv & PAWN_PROM_RANK => 
            for mv in self.pawn_proms(sq).iter() {
                if self.is_legal(mv) {
                    buf.push(mv);
                }
            }
        );
    }

    /// Generate all pseudo-legal non-capturing* moves. No promotions.
    /// 
    /// Note that generated king moves are always legal, and may capture.
    pub fn gen_ncaps(&self, buf: &mut MoveBuffer) {
        self.pawn_ncaps(buf);
        for_sq!(sq in self.knights & self.actv => for m in self.knight_moves(sq, !self.all).iter() { buf.push(m) });
        for_sq!(sq in self.bishops & self.actv => for m in self.bishop_moves(sq, !self.all).iter() { buf.push(m) });
        for_sq!(sq in self.rooks & self.actv   => for m in self.rook_moves  (sq, !self.all).iter() { buf.push(m) });
        for_sq!(sq in self.queens & self.actv  => for m in self.queen_moves (sq, !self.all).iter() { buf.push(m) });
        /* programming is an art! */              for m in self.king_moves  (             ).iter() { buf.push(m) }
    }
    /// Generate non-king pseudo-legal capturing moves. No promotions.
    pub fn gen_caps(&self, buf: &mut MoveBuffer) {
        self.pawn_caps(buf);
        for_sq!(sq in self.knights & self.actv => for m in self.knight_moves(sq, self.idle).iter() { buf.push(m) });
        for_sq!(sq in self.bishops & self.actv => for m in self.bishop_moves(sq, self.idle).iter() { buf.push(m) });
        for_sq!(sq in self.rooks & self.actv   => for m in self.rook_moves  (sq, self.idle).iter() { buf.push(m) });
        for_sq!(sq in self.queens & self.actv  => for m in self.queen_moves (sq, self.idle).iter() { buf.push(m) });
    }

    /// Generates all pseudo-legal moves.
    pub fn gen_all(&self, buf: &mut MoveBuffer) {
        self.gen_proms(buf);
        self.pawn_caps(buf);
        self.pawn_ncaps(buf);
        for_sq!(sq in self.knights & self.actv => for m in self.knight_moves(sq, !self.actv).iter() { buf.push(m) });
        for_sq!(sq in self.bishops & self.actv => for m in self.bishop_moves(sq, !self.actv).iter() { buf.push(m) });
        for_sq!(sq in self.rooks & self.actv   => for m in self.rook_moves  (sq, !self.actv).iter() { buf.push(m) });
        for_sq!(sq in self.queens & self.actv  => for m in self.queen_moves (sq, !self.actv).iter() { buf.push(m) });
        /* programming is an art! */              for m in self.king_moves  (             ).iter() { buf.push(m) }
    }

    /// Generate all legal moves until closure returns true.
    #[inline]
    pub fn for_move<F: FnMut(Move) -> bool>(&self, mut f: F) {
        // while seemingly ripe for refactor, note: return embedding; highly layered closures tank search perf; column associativity -> maintainability
        let aps = self.pawns & self.actv;
        for_sq!(sq in aps &  PAWN_PROM_RANK    => for m in self.pawn_proms  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in aps & !PAWN_PROM_RANK    => for m in self.pawn_moves  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.knights & self.actv => for m in self.knight_moves(sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.bishops & self.actv => for m in self.bishop_moves(sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.rooks & self.actv   => for m in self.rook_moves  (sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        for_sq!(sq in self.queens & self.actv  => for m in self.queen_moves (sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
        /* programming is an art! */              for m in self.king_moves  (  ).iter() { if (&mut f)(m) { return } }
    }
    
    /// Generate all legal moves until closure returns true.
    #[inline]
    pub fn for_role_move<F: FnMut(Move) -> bool>(&self, role: Piece, mut f: F) {
        let aps = self.pawns & self.actv;
        match role { // on the other hand, this function could probably use improvement
            Piece::Pawn =>   { for_sq!(sq in aps &  PAWN_PROM_RANK    => for m in self.pawn_proms  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } });
                               for_sq!(sq in aps & !PAWN_PROM_RANK    => for m in self.pawn_moves  (sq).iter() { if self.is_legal(m) && (&mut f)(m) { return } }); },
            Piece::Knight => { for_sq!(sq in self.knights & self.actv => for m in self.knight_moves(sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } }); },
            Piece::Bishop => { for_sq!(sq in self.bishops & self.actv => for m in self.bishop_moves(sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } }); },
            Piece::Rook =>   { for_sq!(sq in self.rooks & self.actv   => for m in self.rook_moves  (sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } }); },
            Piece::Queen =>  { for_sq!(sq in self.queens & self.actv  => for m in self.queen_moves (sq, !self.actv).iter() { if self.is_legal(m) && (&mut f)(m) { return } }); },
            Piece::King =>   {                                           for m in self.king_moves  (  ).iter() { if                     (&mut f)(m) { return } };  },
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveSet {
    pub to_set: u64,
    pub from: Sq,
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
    pub to_set: u64,
    pub from: Sq,
}

impl PromSet {
    #[inline]
    pub fn iter(&self) -> PromSetIter {
        PromSetIter {
            from: self.from,
            to_set: self.to_set,
            to: Sq::A1, // dummy value - gets reset on first iter
            piece_index: PROM_PIECES.len(),
        }
    }
}


#[derive(Debug, Clone)]
pub struct MoveSetIter {
    to_set: u64,
    from: Sq,
    piece: Piece,
}

impl Iterator for MoveSetIter {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.to_set == 0 { return None; }

        let to = Sq::lsb(self.to_set);
        self.to_set &= self.to_set - 1;

        Some(Move::new(self.from, to, self.piece))
    }
}

/// Iterator over the extrapolated promotion possiblities from a `PromSet`.
#[derive(Debug, Clone)]
pub struct PromSetIter {
    to_set: u64,
    from: Sq,
    to: Sq, 
    piece_index: usize,
}
const PROM_PIECES: [Piece; 4] = [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop];

impl Iterator for PromSetIter {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {

        if self.piece_index == PROM_PIECES.len() {
            if self.to_set == 0 {
                return None;
            }

            self.piece_index = 0;

            self.to = Sq::lsb(self.to_set);
            self.to_set &= self.to_set - 1;
        }
        
        let prom = PROM_PIECES[self.piece_index];
        self.piece_index += 1;
        Some(Move::new(self.from, self.to, prom))
    }
}
