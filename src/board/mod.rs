use std::mem::swap;

pub mod fend;
pub mod mov;
pub mod serde;
pub mod zobrist;


/// Loop through the index of each set bit from least to most significant.
#[macro_export]
macro_rules! for_sq {
    ($sq:ident in $bb:expr => $blk:block) => {
        let mut t = $bb;
        while t != 0 {
            let $sq = t.trailing_zeros() as u8;
            { $blk }
            t &= t - 1; // blsr usually isn't any faster
        }
    }
}
/* /// Loop through each set bit from least to most significant.
#[macro_export]
macro_rules! for_msk {
    ($msk:ident in $bb:expr => $blk:block) => {
        let mut t = $bb;
        while t != 0 {
            let ts1 = t - 1;
            let $msk = t & !ts1;
            { $blk }
            t &= ts1;
        }
    }
} */

/// Flip the rank of a chess square index.
pub const fn flip_sq(sq: u8) -> u8 {
    sq ^ 0o70
}

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
    pub struct CastleFlags: u8 {
        /// Indicates ability to kingside castle.
        const KINGSIDE = 1 << 0;
        /// Indicates ability to queenside castle.
        const QUEENSIDE = 1 << 1;
    }
}


#[derive(Debug, Clone, Copy)]
pub struct Board {
    /// Zobrist hash.
    pub hash: u64,

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
    pub kings: u64,

    /// Bitboard of the square behind `prev` pawn double-advance. Else zero.
    pub en_passant: u64,

    /// Move count since game begin.
    pub move_count: usize,
    /// `1` is white, `-1` is black.
    pub colour: i8,
    /// Count plies to 100 since the last capture or pawn-advance.
    pub fifty_move_clock: u8,
    /// `next`'s castling capabilities.
    pub actv_castle_flags: CastleFlags,
    /// `prev`'s castling capabilities.
    pub idle_castle_flags: CastleFlags,
}

impl Board {

    
    /// Returns the piece at sq and whether it is `actv`'s or not, if there is one.
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

    /// Returns the square index of the active king.
    #[inline]
    pub fn actv_king_sq(&self) -> u8 {
        (self.kings & self.actv).trailing_zeros() as u8
    }
    /// Returns the square index of the idle king.
    #[inline]
    pub fn idle_king_sq(&self) -> u8 {
        (self.kings & self.idle).trailing_zeros() as u8
    }

    pub fn is_actv_in_check(&self) -> bool {
        let sq = self.actv_king_sq();
        fend::knight_fend(sq) & self.knights & self.idle != 0
        || fend::bishop_fend(sq, self.all) & (self.bishops | self.queens) & self.idle != 0
        || fend::rook_fend(sq, self.all) & (self.rooks | self.queens) & self.idle != 0
    }

    pub fn is_idle_in_check(&self) -> bool {
        let sq = self.idle_king_sq();
        fend::knight_fend(sq) & self.knights & self.actv != 0
        || fend::bishop_fend(sq, self.all) & (self.bishops | self.queens) & self.actv != 0
        || fend::rook_fend(sq, self.all) & (self.rooks | self.queens) & self.actv != 0
    }


    /// Play a move on the board, and flip the player turn.
    /// 
    /// Note that legality is not checked, besides
    /// ### Move details:
    /// * `piece` indicated the type of piece moved, unless promoting
    /// a pawn, for which it must be the piece being promoted to.
    /// * `from` and `to` must be bitmasks of the tiles.
    /// * Castling is performed strictly by moving the king two spaces
    /// in a legal fashion, the rook movement is handled automatically.
    /// * En passant is performed automatically.
    /// 
    /// ### Returns:
    /// * `Err(())` - `idle` ends up in check (illegal move, drop self).
    /// * `Ok(())` - `idle` is not in check.
    /// This is an illegal move, and `self` should be dropped.
    pub fn make(&mut self, Move { from_sq, to_sq, piece }: Move) -> Result<(), ()> {
        self.fifty_move_clock += 1;
        let is_actv_white = self.colour == 1;
        let from = 1u64 << from_sq;
        let to = 1u64 << to_sq;

        // handle capture
        if let Some(cap) = self.get_piece_at(to) {
            // remove piece & reset 50-move rule clock
            self.idle &= !to;
            self.fifty_move_clock = 0;
            self.hash ^= zobrist::get_piece_hash_idle(!is_actv_white, cap, to_sq);
            match cap {
                Piece::Pawn => self.pawns &= !to,
                Piece::Knight => self.knights &= !to,
                Piece::Bishop => self.bishops &= !to,
                Piece::Rook => self.rooks &= !to,
                Piece::Queen => self.queens &= !to,
                Piece::King => panic!("king captured!"),
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
            }
        }
        
        self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, piece, from_sq);
        self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, piece, to_sq);
        self.actv = self.actv & !from | to;
        self.en_passant = 0;

        match piece {
            Piece::Pawn => {
                self.pawns = self.pawns & !from | to;
                // reset 50 move clock
                self.fifty_move_clock = 0;
                // double-advance: flag rear tile for en passant
                if to == from << 16 {
                    // shift accounts for board flip
                    self.en_passant = from << 0o40;
                }
            },
            Piece::Knight => self.knights = self.knights & !from | to,
            Piece::Bishop => self.bishops = self.bishops & !from | to,
            Piece::Rook => {
                self.rooks = self.rooks & !from | to;
                if from == 0x1 {
                    self.actv_castle_flags &= !CastleFlags::QUEENSIDE;
                    self.hash ^= zobrist::get_hash_qs_castle(is_actv_white);
                }
                if from == 0x80 {
                    self.actv_castle_flags &= !CastleFlags::KINGSIDE;
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
            }
            Piece::Queen => self.queens = self.queens & !from | to,
            Piece::King => {
                self.kings = self.kings & !from | to;
                if from == to >> 2 { // kingside castle!
                    self.rooks = self.rooks & !0x80 | 0x20;
                } else if from == to << 2 { // queenside castle!
                    self.rooks = self.rooks & !0x1 | 0x8;
                }
                self.actv_castle_flags = CastleFlags::empty();
                if self.actv_castle_flags.contains(CastleFlags::KINGSIDE) {
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
                if self.actv_castle_flags.contains(CastleFlags::KINGSIDE) {
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
            }
        }

        // handle pawn promotions
        self.pawns &= !from;

        // bookkeeping
        self.colour = -self.colour;
        self.move_count += (self.colour + 1 >> 1) as usize;
        swap(&mut self.actv, &mut self.idle);
        swap(&mut self.actv_castle_flags, &mut self.idle_castle_flags);
        self.pawns = self.pawns.swap_bytes();
        self.knights = self.knights.swap_bytes();
        self.bishops = self.bishops.swap_bytes();
        self.rooks = self.rooks.swap_bytes();
        self.queens = self.queens.swap_bytes();
        self.kings = self.kings.swap_bytes();
        self.actv = self.actv.swap_bytes();
        self.idle = self.idle.swap_bytes();

        self.all = self.actv | self.idle;
        self.hash ^= zobrist::COLOUR_HASH;

        // if idle is in check, move is illegal
        if self.is_idle_in_check() { Err(()) } else { Ok(()) }
    }

    /// Check for stalemate and checkmate.
    pub fn is_mate(&self) -> Option<GameOver> {
        let mut move_table = mov::MoveSetTable::new();
        self.get_moveset_actv(&mut move_table);
        
        // if any move avoids check, no mate
        for set in move_table.get_move_sets() {
            for mov in set.iter() {
                let mut board = self.clone();
                if let Ok(_) = board.make(mov) {
                    return None;
                }
            }
        }
        for set in move_table.get_prom_sets() {
            for pro in set.iter() {
                let mut board = self.clone();
                if let Ok(_) = board.make(pro) {
                    return None;
                }
            }
        }

        if self.is_actv_in_check() {
            Some(GameOver::Checkmate)
        } else {
            Some(GameOver::Stalemate)
        }
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

    
    /// Computes the bitmap of 'covered' tiles by `actv`.
    pub fn calc_actv_fend(&self) -> u64 {
        let mut fend = 0;
        fend |= fend::pawns_fend_actv(self.pawns & self.actv);
        fend |= fend::knights_fend(self.knights & self.actv);
        fend |= fend::bishops_fend(self.bishops & self.actv, self.all);
        fend |= fend::rooks_fend(self.rooks & self.actv, self.all);
        fend |= fend::queens_fend(self.queens & self.actv, self.all);
        fend |= fend::king_fend(self.actv_king_sq());
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
        fend |= fend::king_fend(self.idle_king_sq()); 
        fend
    }
}

