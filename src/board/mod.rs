pub mod fend;
pub mod mov;
pub mod fen;
pub mod zobrist;

/// Flip the rank of a chess square index.
pub const fn flip_sq(sq: u8) -> u8 {
    sq ^ 0o70
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub from_sq: u8,
    pub to_sq: u8,
    pub piece: Piece,
}
impl Move {
    pub fn new(from_sq: u8, to_sq: u8, piece: Piece) -> Self {
        Self { from_sq, to_sq, piece }
    }
}
impl std::fmt::Debug for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Move")
            .field("from_sq", &format_args!("{:#o}", self.from_sq))
            .field("to_sq", &format_args!("{:#o}", self.to_sq))
            .field("piece", &self.piece)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Piece {
    King = 0,
    Queen = 1,
    Rook = 2,
    Bishop = 3,
    Knight = 4,
    Pawn = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameOver {
    Checkmate,
    Stalemate,
    ThreefoldRep,
    FiftyMoveRule,
    InsMaterial,
}

bitflags::bitflags! {
    /// Castle capabilities.
    pub struct CastleRights: u8 {
        /// Indicates ability to kingside castle.
        const KINGSIDE = 1 << 0;
        /// Indicates ability to queenside castle.
        const QUEENSIDE = 1 << 1;
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Board {
    /// Bitboard of all pieces.
    pub all: u64,
    /// Bitboard of the pieces of the colour to move.
    pub actv: u64,
    /// Bitboard of the pieces of the colour that moved.
    pub idle: u64,

    pub pawns: u64,
    pub bishops: u64,
    pub knights: u64,
    pub rooks: u64,
    pub queens: u64,
    pub actv_king_sq: u8,
    pub idle_king_sq: u8,

    /// Bitboard of the square behind `prev` pawn double-advance. Else zero.
    pub en_passant: u64,
    /// Zobrist hash.
    pub hash: u64,

    /// Move count since game begin.
    pub move_count: u16,
    /// Count plies to 100 since the last capture or pawn-advance.
    pub fifty_move_clock: u16,
    /// `1` is white, `-1` is black.
    pub colour: i8,
    /// `next`'s castling capabilities.
    pub actv_castle_rights: CastleRights,
    /// `prev`'s castling capabilities.
    pub idle_castle_rights: CastleRights,
}


/* #[derive(Debug, Clone, Copy)]
pub enum Capture {
    Normal(Piece),
    EnPassant,
}

#[derive(Debug, Clone, Copy)]
pub struct Unmake {
    pub pawn_move: bool,
    pub capture: Option<Capture>,

    /// Bitboard of the square behind `prev` pawn double-advance. Else zero.
    pub en_passant: u64,
    /// Zobrist hash.
    pub hash: u64,

    /// Count plies to 100 since the last capture or pawn-advance.
    pub fifty_move_clock: u16,
    /// `next`'s castling capabilities.
    pub actv_castle_flags: CastleFlags,
    /// `prev`'s castling capabilities.
    pub idle_castle_flags: CastleFlags,
} */

impl Board {
    /// Returns the piece at sq, if there is one.
    pub fn get_piece_at(&self, mask: u64) -> Option<Piece> {
        if self.all & mask == 0 {
            None
        } else {
            Some(  if self.pawns & mask != 0 {
                Piece::Pawn
            } else if self.knights & mask != 0 {
                Piece::Knight
            } else if self.bishops & mask != 0 {
                Piece::Bishop
            } else if self.rooks & mask != 0 {
                Piece::Rook
            } else if self.queens & mask != 0 {
                Piece::Queen
            } else { // king
                Piece::King
            })
        }
    }

    fn is_tile_cvrd_actv(&self, sq: u8, all: u64, actv: u64) -> bool {
        fend::knight_fend(sq) & self.knights & actv != 0
        || fend::bishop_fend(sq, all) & (self.bishops | self.queens) & actv != 0
        || fend::rook_fend(sq, all) & (self.rooks | self.queens) & actv != 0
        || fend::pawn_fend_idle(sq) & self.pawns & actv != 0
        || fend::king_fend(sq) & (1 << self.actv_king_sq) != 0
    }
    fn is_tile_cvrd_idle(&self, sq: u8, all: u64, idle: u64) -> bool {
        fend::knight_fend(sq) & self.knights & idle != 0
        || fend::bishop_fend(sq, all) & (self.bishops | self.queens) & idle != 0
        || fend::rook_fend(sq, all) & (self.rooks | self.queens) & idle != 0
        || fend::pawn_fend_actv(sq) & self.pawns & idle != 0
        || fend::king_fend(sq) & (1 << self.idle_king_sq) != 0
    }

    #[inline]
    pub fn is_actv_in_check(&self) -> bool {
        self.is_tile_cvrd_idle(self.actv_king_sq, self.all, self.idle)
    }
    /// If this ever returns true, this is a bug.
    #[inline]
    pub fn is_idle_in_check(&self) -> bool {
        self.is_tile_cvrd_actv(self.idle_king_sq, self.all, self.actv)
    }

    /// Flip the board. Does not update move counters.
    #[inline]
    pub fn flip(&mut self) {
        self.colour = -self.colour;
        
        self.pawns = self.pawns.swap_bytes();
        self.knights = self.knights.swap_bytes();
        self.bishops = self.bishops.swap_bytes();
        self.rooks = self.rooks.swap_bytes();
        self.queens = self.queens.swap_bytes();
        (self.actv_king_sq, self.idle_king_sq) = (flip_sq(self.idle_king_sq), flip_sq(self.actv_king_sq));
        (self.actv, self.idle) = (self.idle.swap_bytes(), self.actv.swap_bytes());
        (self.actv_castle_rights, self.idle_castle_rights) = (self.idle_castle_rights, self.actv_castle_rights);

        self.all = self.actv | self.idle;
        self.hash ^= zobrist::COLOUR_HASH;
    }


    /// Play a move on the board, and flip the player turn.
    /// 
    /// Note that legality is not checked.
    pub fn make(&mut self, Move { from_sq, to_sq, piece }: Move) /* -> Unmake */ {
        let is_actv_white = self.colour == 1;
        let from = 1u64 << from_sq;
        let to = 1u64 << to_sq;

        /* let mut unmake = Unmake {
            pawn_move: false,
            capture: None,
            en_passant: self.en_passant,
            hash: self.hash,
            fifty_move_clock: self.fifty_move_clock,
            actv_castle_flags: self.actv_castle_flags,
            idle_castle_flags: self.idle_castle_flags,
        }; */
        
        self.fifty_move_clock += 1;
        
        // handle capture
        if let Some(cap) = self.get_piece_at(to) {
            // remove piece & reset 50-move rule clock
            self.idle &= !to;
            self.fifty_move_clock = 0;
            self.hash ^= zobrist::get_piece_hash_idle(!is_actv_white, cap, to_sq);
            /* unmake.capture = Some(Capture::Normal(cap)); */
            match cap {
                Piece::Pawn => self.pawns &= !to,
                Piece::Knight => self.knights &= !to,
                Piece::Bishop => self.bishops &= !to,
                Piece::Queen => self.queens &= !to,
                Piece::Rook => {
                    self.rooks &= !to;
                    if to == 0x100000000000000 {
                        if self.idle_castle_rights.contains(CastleRights::QUEENSIDE) {
                            self.idle_castle_rights &= !CastleRights::QUEENSIDE;
                            self.hash ^= zobrist::get_hash_qs_castle(!is_actv_white);
                        }
                    } else if to == 0x8000000000000000 {
                        if self.idle_castle_rights.contains(CastleRights::KINGSIDE) {
                            self.idle_castle_rights &= !CastleRights::KINGSIDE;
                            self.hash ^= zobrist::get_hash_ks_castle(!is_actv_white);
                        }
                    }
                }
                Piece::King => panic!("king captured! fen: {} | {:?} | {:?}", self.to_fen(), Move::new(from_sq, to_sq, piece), &self),
            }
        } else {
            // check for en passant before assuming no capture
            if piece == Piece::Pawn && to == self.en_passant {
                let cap = to >> 8;
                self.pawns &= !cap;
                self.idle &= !cap;
                self.fifty_move_clock = 0;
                self.hash ^= zobrist::get_piece_hash_idle(
                    !is_actv_white,
                    Piece::Pawn,
                    to_sq - 0o10
                );
                /* unmake.capture = Some(Capture::EnPassant); */
            }
        }
        
        if self.en_passant != 0 {
            self.hash ^= zobrist::get_hash_en_passant(self.en_passant);
            self.en_passant = 0; // clear between potential read and write
        }

        match piece {
            Piece::Pawn => {
                self.pawns = self.pawns & !from | to;
                // reset 50 move clock
                self.fifty_move_clock = 0;
                // double-advance: flag rear tile for en passant
                if to == from << 16 {
                    // shift accounts for board flip
                    self.en_passant = from << 0o40;
                    self.hash ^= zobrist::get_hash_en_passant(self.en_passant);
                }
            }
            Piece::Knight => self.knights = self.knights & !from | to,
            Piece::Bishop => self.bishops = self.bishops & !from | to,
            Piece::Queen =>  self.queens  = self.queens  & !from | to,
            Piece::Rook => {
                self.rooks = self.rooks & !from | to;
                if from == 0x1 {
                    if self.actv_castle_rights.contains(CastleRights::QUEENSIDE) {
                        self.actv_castle_rights &= !CastleRights::QUEENSIDE;
                        self.hash ^= zobrist::get_hash_qs_castle(is_actv_white);
                    }
                } else if from == 0x80 {
                    if self.actv_castle_rights.contains(CastleRights::KINGSIDE) {
                        self.actv_castle_rights &= !CastleRights::KINGSIDE;
                        self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                    }
                }
            },
            Piece::King => {
                self.actv_king_sq = to_sq;
                if from == to >> 2 { // kingside castle!
                    self.rooks = self.rooks & !0x80 | 0x20;
                    self.actv = self.actv & !0x80 | 0x20;
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o7);
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o5);
                } else if from == to << 2 { // queenside castle!
                    self.rooks = self.rooks & !0x1 | 0x8;
                    self.actv = self.actv & !0x1 | 0x8;
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o0);
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o3);
                }
                if self.actv_castle_rights.contains(CastleRights::KINGSIDE) {
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
                if self.actv_castle_rights.contains(CastleRights::QUEENSIDE) {
                    self.hash ^= zobrist::get_hash_qs_castle(is_actv_white);
                }
                self.actv_castle_rights = CastleRights::empty();
            }
        }
        
        // update actv
        self.actv = self.actv & !from | to;
        // update hash & handle pawn promotions
        if self.pawns & from != 0 {
            /* unmake.pawn_move = true; */
            self.pawns &= !from;
            self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Pawn, from_sq);
        } else {
            self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, piece, from_sq);
        }
        self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, piece, to_sq);

        // bookkeeping
        self.flip();
        self.move_count += (self.colour + 1) as u16 >> 1;

        /* unmake */
    }

    /* pub fn unmake(&mut self, Move { from_sq, to_sq, piece }: Move, unmake: &Unmake) {
        let from = 1u64 << from_sq;
        let to = 1u64 << to_sq;

        self.move_count -= (self.colour + 1) as u16 >> 1;
        self.flip();

        // update actv
        self.actv = self.actv & !to | from;
        // handle pawn promotions
        if unmake.pawn_move && piece != Piece::Pawn {
            self.pawns |= from;
            match piece {
                Piece::Queen =>  self.queens &= !to,
                Piece::Rook =>   self.rooks &= !to,
                Piece::Bishop => self.bishops &= !to,
                Piece::Knight => self.knights &= !to,
                _ => panic!(),
            }
        }

        // update moved piece
        match piece {
            Piece::Pawn =>   self.pawns = self.pawns & !to | from,
            Piece::Knight => self.knights = self.knights & !to | from,
            Piece::Bishop => self.bishops = self.bishops & !to | from,
            Piece::Queen =>  self.queens = self.queens & !to | from,
            Piece::Rook =>   self.rooks = self.rooks & !to | from,
            Piece::King => {
                self.actv_king_sq = from_sq;
                if from == to >> 2 { // kingside castle!
                    self.rooks = self.rooks & !0x20 | 0x80;
                    self.actv = self.actv & !0x20 | 0x80;
                } else if from == to << 2 { // queenside castle!
                    self.rooks = self.rooks & !0x8 | 0x1;
                    self.actv = self.actv & !0x8 | 0x1;
                }
            }
        }

        // handle capture
        if let Some(cap) = unmake.capture {
            match cap {
                Capture::Normal(piece) => {
                    self.idle |= to;
                    match piece {
                        Piece::Pawn => self.pawns |= to,
                        Piece::Knight => self.knights |= to,
                        Piece::Bishop => self.bishops |= to,
                        Piece::Rook => self.rooks |= to,
                        Piece::Queen => self.queens |= to,
                        Piece::King => unreachable!(),
                    }
                },
                Capture::EnPassant => {
                    let cap = to >> 8;
                    self.pawns |= cap;
                    self.idle |= cap;
                },
            }
        }

        self.all = self.actv | self.idle;

        self.actv_castle_flags = unmake.actv_castle_flags;
        self.idle_castle_flags = unmake.idle_castle_flags;
        self.fifty_move_clock = unmake.fifty_move_clock;
        self.hash = unmake.hash;
        self.en_passant = unmake.en_passant;
    } */


    /// Play a null move and flip the player turn.
    pub fn make_null(&mut self) {
        self.flip();
        self.move_count += (self.colour + 1) as u16 >> 1;
        self.fifty_move_clock += 1;
    }
    /// Undo a null move and flip the player turn.
    pub fn unmake_null(&mut self, en_passant: u64) {
        self.flip();
        self.move_count -= (self.colour + 1) as u16 >> 1;
        self.fifty_move_clock -= 1;
        self.en_passant = en_passant;
    }

    /// Check for stalemate and checkmate.
    pub fn is_mate(&self) -> Option<GameOver> {
        // if any move can be legally played, no mate
        let mut legal_move_exists = false;
        self.for_mov(|_| { legal_move_exists = true; true });
        if legal_move_exists { return None; }
        
        Some(match self.is_actv_in_check() {
            true => GameOver::Checkmate,
            false => GameOver::Stalemate,
        })
    }
    /// Check for fifty move rule and insufficient material.
    /// 
    /// Note: does not check threefold repetition.
    pub fn is_draw(&self) -> Option<GameOver> {
        // 50 move rule
        if self.fifty_move_clock >= 100 {
            return Some(GameOver::FiftyMoveRule);
        }
        
        // insufficient material
        if self.pawns | self.rooks | self.queens == 0 {
            let minors = self.knights | self.bishops;
            let actv_minor_thresh = (self.actv & minors).count_ones() <= 1;
            let idle_minor_thresh = (self.idle & minors).count_ones() <= 1;
            
            if actv_minor_thresh && idle_minor_thresh {
                // kx v kx, where x is a minor piece or nothing
                return Some(GameOver::InsMaterial);
            } else if actv_minor_thresh || idle_minor_thresh {
                if self.bishops == 0 && self.knights == 2 {
                    // knn v k
                    return Some(GameOver::InsMaterial);
                }
            }
        }

        None
    }
    
    /// Check if the game is over.
    /// 
    /// Note: does not check threefold repetition.
    pub fn is_game_over(&self) -> Option<GameOver> {
        if let Some(r) = self.is_draw() { return Some(r) }
        self.is_mate()
    }

    
    /* /// Computes the bitmap of 'covered' tiles by `actv`.
    pub fn calc_actv_fend(&self) -> u64 {
        let mut fend = 0;
        fend |= fend::pawns_fend_actv(self.pawns & self.actv);
        fend |= fend::knights_fend(self.knights & self.actv);
        fend |= fend::bishops_fend(self.bishops & self.actv, self.all);
        fend |= fend::rooks_fend(self.rooks & self.actv, self.all);
        fend |= fend::queens_fend(self.queens & self.actv, self.all);
        fend |= fend::king_fend(self.actv_king_sq);
        fend
    }
    /// Computes the bitmap of 'covered' tiles by `idle`.
    pub fn calc_idle_fend(&mut self) -> u64 {
        let mut fend = 0;
        fend |= fend::pawns_fend_idle(self.pawns & self.idle);
        fend |= fend::knights_fend(self.knights & self.idle);
        fend |= fend::bishops_fend(self.bishops & self.idle, self.all);
        fend |= fend::rooks_fend(self.rooks & self.idle, self.all);
        fend |= fend::queens_fend(self.queens & self.idle, self.all);
        fend |= fend::king_fend(self.idle_king_sq); 
        fend
    } */

    /// Check if `self` is in a valid state.
    pub fn validate(&self) -> Result<(), &'static str> {
        use crate::as_result;

        // validate king square data
        as_result!(self.idle_king_sq < 64)?;
        as_result!(1 << self.idle_king_sq & self.idle != 0)?;
        as_result!(self.actv_king_sq < 64)?;
        as_result!(1 << self.actv_king_sq & self.actv != 0)?;

        // validate bitboard cohesion
        as_result!(self.actv & self.idle == 0)?;
        as_result!(self.all == self.actv | self.idle)?;

        // validate against piece role overlap
        let mut overlap = self.pawns;
        as_result!(self.knights & overlap == 0)?;
        overlap |= self.knights;
        as_result!(self.bishops & overlap == 0)?;
        overlap |= self.bishops;
        as_result!(self.rooks & overlap == 0)?;
        overlap |= self.rooks;
        as_result!(self.queens & overlap == 0)?;
        overlap |= self.queens;
        as_result!(1 << self.actv_king_sq & overlap == 0)?;
        overlap |= 1 << self.actv_king_sq;
        as_result!(1 << self.idle_king_sq & overlap == 0)?;

        // validate pawn positions and en passant
        as_result!(self.pawns & 0xFF000000000000FF == 0)?;
        as_result!(self.en_passant.count_ones() <= 1)?;
        as_result!((self.en_passant >> 0o10 & self.pawns & self.idle & 0xFF00000000) << 0o10 == self.en_passant)?;

        // validate castle rights as best as possible
        as_result!(!self.actv_castle_rights.contains(CastleRights::KINGSIDE)  || self.actv_king_sq == 0o4)?;
        as_result!(!self.actv_castle_rights.contains(CastleRights::KINGSIDE)  || self.rooks & self.actv & 0x80 != 0)?;
        as_result!(!self.actv_castle_rights.contains(CastleRights::QUEENSIDE) || self.actv_king_sq == 0o4)?;
        as_result!(!self.actv_castle_rights.contains(CastleRights::QUEENSIDE) || self.rooks & self.actv & 0x1 != 0)?;
        as_result!(!self.idle_castle_rights.contains(CastleRights::KINGSIDE)  || self.idle_king_sq == 0o74)?;
        as_result!(!self.idle_castle_rights.contains(CastleRights::KINGSIDE)  || self.rooks & self.idle & 0x8000000000000000 != 0)?;
        as_result!(!self.idle_castle_rights.contains(CastleRights::QUEENSIDE) || self.idle_king_sq == 0o74)?;
        as_result!(!self.idle_castle_rights.contains(CastleRights::QUEENSIDE) || self.rooks & self.idle & 0x100000000000000 != 0)?;

        // validate position legality
        as_result!(!self.is_idle_in_check())?;

        // validate misc data as best as possible
        as_result!(self.colour.abs() == 1)?;
        as_result!(self.hash == self.calc_hash())?;
        as_result!(self.fifty_move_clock <= (self.move_count - 1) * 2 + 1)?;

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    /* #[test]
    fn test_make_unmake() {
        let fen1 = "r1bqk1nr/pppp1ppp/2B5/2b1p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQ - 0 4";
        let fen2 = "R7/6k1/8/8/P6P/6K1/8/4r3 b - - 0 1";

        let mut board = Board::from_fen(fen1).unwrap();
        let comparison = board.clone();
        let mov = Move::new(0o6, 0o25, Piece::Knight);
        let unmake = board.make(mov);
        board.unmake(mov, &unmake);
        assert_eq!(board, comparison);

        let mut board = Board::from_fen(fen2).unwrap();
        let comparison = board.clone();
        let mov = Move::new(0o74, 0o76, Piece::Rook);
        let unmake = board.make(mov);
        board.unmake(mov, &unmake);
        assert_eq!(board, comparison);
    } */

    /* #[test]
    fn test_make_is_move_legal() {
        let fen1 = "r1bq1r1k/ppp2B1p/3n1n1b/4NPp1/4P3/3P4/PPPK2PP/RNBQ3R w - g6 0 1"; // en passant
        let fen2 = "r1bq1rk1/ppp1bB1p/3n1n2/4N3/4P1p1/3P4/PPP2PPP/RNBQ1RK1 b - - 0 1"; // captures and defends
        let fen3 = "r1bq1r1k/ppp2B1p/5nb1/4NPp1/1B2P3/1QN5/PPP3PP/R3K2R w KQ - 0 1"; // castling
        let fen4 = "6k1/5pp1/p2Np3/4P2p/1P5n/P7/1N3P2/3R2Kr w - - 4 35";

        let b = Board::from_fen(fen4).unwrap();
        assert!(b.is_move_legal(Move::new(6, 7, Piece::King)));

        for fen in [fen1, fen2, fen3, fen4] {
            dbg!(&fen);
            let board = Board::from_fen(fen).unwrap();
            board.validate().unwrap();
            let mut tab = mov::MoveSetTable::new();
            board.get_move_tab_actv(&mut tab);
            for_mov!(mov in tab => {
                if board.is_move_legal(mov) {
                    dbg!(mov);
                    let mut board = board.clone();
                    board.make(mov);
                    /* dbg!(&board); */
                    dbg!(&board.to_fen());
                    assert!(!board.is_idle_in_check());
                    board.validate().unwrap();
                } else {
                    let mut board = board.clone();
                    board.make(mov);
                    /* dbg!(&board); */
                    assert!(board.is_idle_in_check());
                }
            });
        }
    } */
}
