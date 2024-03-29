pub mod fend;
pub mod mov;
pub mod zobrist;
pub mod eval;

use crate::{Sq, Side, Piece, Move, CastleRights, GameOver, board::eval::{IDLE, ACTV}, PAWN_IDX, ROOK_IDX};


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
    pub actv_king: Sq,
    pub idle_king: Sq,

    /// Bitboard of the square behind `prev` pawn double-advance. Else zero.
    pub en_passant: u64,
    /// Position zobrist hash.
    pub hash: u64,

    /// Side to move.
    pub side: Side,
    /// Plies since game begin.
    pub total_ply_number: u16,
    /// Count plies to 100 since the last capture or pawn-advance.
    pub fifty_move_clock: u16,
    /// `actv`'s castling capabilities.
    pub actv_castle_rights: CastleRights,
    /// `idle`'s castling capabilities.
    pub idle_castle_rights: CastleRights,

    pub eval: [i16; 2],
    pub phase: u8,
}

impl Board {
    /// Returns the piece at sq, if there is one.
    pub fn fullmove_number(&self) -> u16 {
        self.total_ply_number / 2 + 1
    }

    /// Returns the piece at sq, if there is one.
    pub fn get_piece_at(&self, sq: Sq) -> Option<Piece> {
        let bm = sq.bm();
        if self.all & bm == 0 {
            None
        } else {
            Some(  if self.pawns & bm != 0 {
                Piece::Pawn
            } else if self.knights & bm != 0 {
                Piece::Knight
            } else if self.bishops & bm != 0 {
                Piece::Bishop
            } else if self.rooks & bm != 0 {
                Piece::Rook
            } else if self.queens & bm != 0 {
                Piece::Queen
            } else { // king
                Piece::King
            })
        }
    }

    fn is_tile_cvrd_actv(&self, sq: Sq) -> bool {
        fend::knight_fend(sq) & self.knights & self.actv != 0
        || fend::bishop_fend(sq, self.all) & (self.bishops | self.queens) & self.actv != 0
        || fend::rook_fend(sq, self.all) & (self.rooks | self.queens) & self.actv != 0
        || fend::pawn_fend_idle(sq) & self.pawns & self.actv != 0
        || fend::king_fend(sq) & self.actv_king.bm() != 0
    }
    fn is_tile_cvrd_idle(&self, sq: Sq) -> bool {
        fend::knight_fend(sq) & self.knights & self.idle != 0
        || fend::bishop_fend(sq, self.all) & (self.bishops | self.queens) & self.idle != 0
        || fend::rook_fend(sq, self.all) & (self.rooks | self.queens) & self.idle != 0
        || fend::pawn_fend_actv(sq) & self.pawns & self.idle != 0
        || fend::king_fend(sq) & self.idle_king.bm() != 0
    }

    #[inline]
    pub fn in_check(&self) -> bool {
        self.is_tile_cvrd_idle(self.actv_king)
    }
    /// If this ever returns true, a bug has occured.
    #[inline]
    pub fn is_idle_in_check(&self) -> bool {
        self.is_tile_cvrd_actv(self.idle_king)
    }

    /// Flip the board back/forth.
    #[inline]
    pub fn flip(&mut self) {
        self.side = self.side.flip();
        
        self.pawns = self.pawns.swap_bytes();
        self.knights = self.knights.swap_bytes();
        self.bishops = self.bishops.swap_bytes();
        self.rooks = self.rooks.swap_bytes();
        self.queens = self.queens.swap_bytes();
        (self.actv_king, self.idle_king) = (self.idle_king.flip(), self.actv_king.flip());
        (self.actv, self.idle) = (self.idle.swap_bytes(), self.actv.swap_bytes());
        (self.actv_castle_rights, self.idle_castle_rights) = (self.idle_castle_rights, self.actv_castle_rights);
        self.eval[0] = -self.eval[0];
        self.eval[1] = -self.eval[1];

        self.all = self.actv | self.idle;
        self.hash ^= zobrist::COLOUR_HASH;
    }


    /// Play a null move and flip the player turn.
    #[inline]
    pub fn clone_make(&self, mv: Move) -> Self {
        let mut clone = self.clone();
        clone.make(mv);
        clone
    }
    /// Play a move on the board, and flip the player turn.
    /// 
    /// Note that legality is not checked.
    pub fn make(&mut self, Move { from, to, piece }: Move) {
        let from_bm = from.bm();
        let to_bm = to.bm();

        self.fifty_move_clock += 1;
        
        // handle capture
        if let Some(cap) = self.get_piece_at(to) {
            // remove piece & reset 50-move rule clock
            self.idle &= !to_bm;
            self.fifty_move_clock = 0;
            self.hash ^= zobrist::get_piece_hash_idle(!self.side, cap, to);
            self.eval_sub(eval::PSQT[IDLE][cap as usize][to.us()]);
            self.phase -= eval::PHASE_ADJUST[cap as usize];

            match cap {
                Piece::Pawn => self.pawns &= !to_bm,
                Piece::Knight => self.knights &= !to_bm,
                Piece::Bishop => self.bishops &= !to_bm,
                Piece::Queen => self.queens &= !to_bm,
                Piece::Rook => {
                    self.rooks &= !to_bm;
                    if to_bm == 0x100000000000000 {
                        if self.idle_castle_rights.queenside() {
                            self.idle_castle_rights.void_queenside();
                            self.hash ^= zobrist::get_hash_qs_castle(!self.side);
                        }
                    } else if to_bm == 0x8000000000000000 {
                        if self.idle_castle_rights.kingside() {
                            self.idle_castle_rights.void_kingside();
                            self.hash ^= zobrist::get_hash_ks_castle(!self.side);
                        }
                    }
                }
                Piece::King => unreachable!(),
            }
        } else {
            // check for en passant before assuming no capture
            if piece == Piece::Pawn && to_bm == self.en_passant {
                let cap = to_bm >> 8;
                self.pawns &= !cap;
                self.idle &= !cap;
                self.fifty_move_clock = 0;
                self.hash ^= zobrist::get_piece_hash_idle(
                    !self.side,
                    Piece::Pawn,
                    Sq::lsb(to.bm() >> 0o10)
                );
                self.eval_sub(eval::PSQT[IDLE][PAWN_IDX][to.us() - 8]);
            }
        }
        
        if self.en_passant != 0 {
            self.hash ^= zobrist::get_hash_en_passant(self.en_passant);
            self.en_passant = 0; // clear between potential read and write
        }

        match piece {
            Piece::Pawn => {
                self.pawns = self.pawns & !from_bm | to_bm;
                // reset 50 move clock
                self.fifty_move_clock = 0;
                // double-advance: flag rear tile for en passant
                if to_bm == from_bm << 16 {
                    debug_assert!(from_bm & 0xFF00 != 0);
                    self.en_passant = from_bm << 0o40; // shift accounts for board flip
                    self.hash ^= zobrist::get_hash_en_passant(self.en_passant);
                }
            }
            Piece::Knight => self.knights = self.knights & !from_bm | to_bm,
            Piece::Bishop => self.bishops = self.bishops & !from_bm | to_bm,
            Piece::Queen =>  self.queens  = self.queens  & !from_bm | to_bm,
            Piece::Rook => {
                self.rooks = self.rooks & !from_bm | to_bm;
                if from_bm == 0x1 {
                    if self.actv_castle_rights.queenside() {
                        self.actv_castle_rights.void_queenside();
                        self.hash ^= zobrist::get_hash_qs_castle(self.side);
                    }
                } else if from_bm == 0x80 {
                    if self.actv_castle_rights.kingside() {
                        self.actv_castle_rights.void_kingside();
                        self.hash ^= zobrist::get_hash_ks_castle(self.side);
                    }
                }
            },
            Piece::King => {
                self.actv_king = to;
                if from_bm == to_bm >> 2 { // kingside castle!
                    self.rooks = self.rooks & !0x80 | 0x20;
                    self.actv = self.actv & !0x80 | 0x20;
                    self.hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::H1);
                    self.eval_sub(eval::PSQT[ACTV][ROOK_IDX][Sq::H1.us()]);
                    self.hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::F1);
                    self.eval_add(eval::PSQT[ACTV][ROOK_IDX][Sq::F1.us()]);
                } else if from_bm == to_bm << 2 { // queenside castle!
                    self.rooks = self.rooks & !0x1 | 0x8;
                    self.actv = self.actv & !0x1 | 0x8;
                    self.hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::A1);
                    self.eval_sub(eval::PSQT[ACTV][ROOK_IDX][Sq::A1.us()]);
                    self.hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::D1);
                    self.eval_add(eval::PSQT[ACTV][ROOK_IDX][Sq::D1.us()]);
                }
                if self.actv_castle_rights.kingside() {
                    self.hash ^= zobrist::get_hash_ks_castle(self.side);
                }
                if self.actv_castle_rights.queenside() {
                    self.hash ^= zobrist::get_hash_qs_castle(self.side);
                }
                self.actv_castle_rights = CastleRights::new(false, false);
            }
        }
        
        // update actv
        self.actv = self.actv & !from_bm | to_bm;

        // update hash & handle pawn promotions
        if self.pawns & from_bm != 0 && piece != Piece::Pawn {
            self.pawns &= !from_bm;
            self.hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Pawn, from);
            self.eval_sub(eval::PSQT[ACTV][PAWN_IDX][from.us()]);
            self.phase -= eval::PHASE_ADJUST[PAWN_IDX];
            self.phase += eval::PHASE_ADJUST[piece as usize];
        } else {
            self.hash ^= zobrist::get_piece_hash_actv(self.side, piece, from);
            self.eval_sub(eval::PSQT[ACTV][piece as usize][from.us()]);
        }
        self.hash ^= zobrist::get_piece_hash_actv(self.side, piece, to);
        self.eval_add(eval::PSQT[ACTV][piece as usize][to.us()]);

        // bookkeeping
        self.flip();
        self.total_ply_number += 1;

        debug_assert!(self.validate().is_ok());
    }

    /// Calculate the hash for the position resulting from the move.
    /// 
    /// Note that legality is not checked.
    pub fn make_hash(&self, Move { from, to, piece }: Move) -> u64 {
        let mut hash = self.hash;

        let from_bm = from.bm();
        let to_bm = to.bm();
        
        // handle capture
        if let Some(cap) = self.get_piece_at(to) {
            hash ^= zobrist::get_piece_hash_idle(!self.side, cap, to);
        } else {
            // check for en passant before assuming no capture
            if piece == Piece::Pawn && to_bm == self.en_passant {
                hash ^= zobrist::get_piece_hash_idle(self.side, Piece::Pawn, Sq::lsb(to.bm() >> 0o10));
            }
        }
        
        if self.en_passant != 0 {
            hash ^= zobrist::get_hash_en_passant(self.en_passant);
        }

        match piece {
            Piece::Pawn => {
                // double-advance: flag rear tile for en passant
                if to_bm == from_bm << 16 {
                    hash ^= zobrist::get_hash_en_passant(self.en_passant);
                }
            }
            Piece::Rook => {
                if from_bm == 0x1 {
                    if self.actv_castle_rights.queenside() {
                        hash ^= zobrist::get_hash_qs_castle(self.side);
                    }
                } else if from_bm == 0x80 {
                    if self.actv_castle_rights.kingside() {
                        hash ^= zobrist::get_hash_ks_castle(self.side);
                    }
                }
            },
            Piece::King => {
                if from_bm == to_bm >> 2 { // kingside castle!
                    hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::H1);
                    hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::F1);
                } else if from_bm == to_bm << 2 { // queenside castle!
                    hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::A1);
                    hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Rook, Sq::D1);
                }
                if self.actv_castle_rights.kingside() {
                    hash ^= zobrist::get_hash_ks_castle(self.side);
                }
                if self.actv_castle_rights.queenside() {
                    hash ^= zobrist::get_hash_qs_castle(self.side);
                }
            }
            _ => (),
        }
        
        // update hash & handle pawn promotions
        if self.pawns & from_bm != 0 {
            hash ^= zobrist::get_piece_hash_actv(self.side, Piece::Pawn, from);
        } else {
            hash ^= zobrist::get_piece_hash_actv(self.side, piece, from);
        }
        hash ^= zobrist::get_piece_hash_actv(self.side, piece, to);

        hash
    }

    /// Play a null move and flip the player turn.
    #[inline]
    pub fn clone_make_null(&self) -> Self {
        let mut clone = self.clone();
        clone.make_null();
        clone
    }
    /// Play a null move and flip the player turn.
    pub fn make_null(&mut self) {
        self.flip();
        self.total_ply_number += 1;
        self.fifty_move_clock += 1;

        if self.en_passant != 0 {
            self.hash ^= zobrist::get_hash_en_passant(self.en_passant);
            self.en_passant = 0;
        }
        
        debug_assert!(self.validate().is_ok());
    }


    /// Check for stalemate and checkmate.
    pub fn is_mate(&self) -> Option<GameOver> {
        // if any move can be legally played, no mate
        let mut legal_move_exists = false;
        self.for_move(|_| { legal_move_exists = true; true });
        if legal_move_exists { return None; }
        
        Some(match self.in_check() {
            true => GameOver::Checkmate,
            false => GameOver::Stalemate,
        })
    }

    pub fn is_draw_50_move(&self) -> bool {
        self.fifty_move_clock >= 100
    }

    pub fn is_draw_insf_mtrl(&self) -> bool {
        if self.pawns | self.rooks | self.queens == 0 {
            let minors = self.knights | self.bishops;
            let actv_minor_thresh = (self.actv & minors).count_ones() <= 1;
            let idle_minor_thresh = (self.idle & minors).count_ones() <= 1;
            
            // kx v kx, where x is a minor piece or nothing
            actv_minor_thresh && idle_minor_thresh || 
            // knn v k
            (actv_minor_thresh || idle_minor_thresh) && self.bishops == 0 && self.knights == 2
        } else {
            false
        }
    }
    
    /// Check if the game is over.
    /// 
    /// Note: does not check threefold repetition.
    pub fn is_game_over(&self) -> Option<GameOver> {
        if self.is_draw_50_move() { return Some(GameOver::FiftyMoveRule) }
        if self.is_draw_insf_mtrl() { return Some(GameOver::InsufficientMaterial) }
        self.is_mate()
    }


    /// Check if `self` is in a valid state.
    pub fn validate(&self) -> Result<(), &'static str> {
        use crate::as_result;

        // validate king square data
        as_result!(self.idle_king.bm() & self.idle != 0)?;
        as_result!(self.actv_king.bm() & self.actv != 0)?;

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
        as_result!(self.actv_king.bm() & overlap == 0)?;
        overlap |= self.actv_king.bm();
        as_result!(self.idle_king.bm() & overlap == 0)?;
        overlap |= self.idle_king.bm();
        as_result!(self.all == overlap)?;

        // validate pawn positions and en passant
        as_result!(self.pawns & 0xFF000000000000FF == 0)?;
        as_result!(self.en_passant.count_ones() <= 1)?;
        as_result!((self.en_passant >> 0o10 & self.pawns & self.idle & 0xFF00000000) << 0o10 == self.en_passant)?;

        // validate castle rights as best as possible
        as_result!(!self.actv_castle_rights.kingside()  || self.actv_king == Sq::E1)?;
        as_result!(!self.actv_castle_rights.kingside()  || self.rooks & self.actv & 0x80 != 0)?;
        as_result!(!self.actv_castle_rights.queenside() || self.actv_king == Sq::E1)?;
        as_result!(!self.actv_castle_rights.queenside() || self.rooks & self.actv & 0x1 != 0)?;
        as_result!(!self.idle_castle_rights.kingside()  || self.idle_king == Sq::E8)?;
        as_result!(!self.idle_castle_rights.kingside()  || self.rooks & self.idle & 0x8000000000000000 != 0)?;
        as_result!(!self.idle_castle_rights.queenside() || self.idle_king == Sq::E8)?;
        as_result!(!self.idle_castle_rights.queenside() || self.rooks & self.idle & 0x100000000000000 != 0)?;

        // validate position legality
        as_result!(!self.is_idle_in_check())?;

        // validate misc data as best as possible
        as_result!(self.hash == zobrist::compute_hash(&self))?;
        as_result!((self.eval, self.phase) == eval::calc_eval_phase(self))?;
        as_result!(self.fifty_move_clock <= self.total_ply_number)?;

        Ok(())
    }
}

