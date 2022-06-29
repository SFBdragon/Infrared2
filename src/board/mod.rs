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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    /// Bitboard of all pieces.
    pub all: u64,
    /// Bitboard of the pieces of the colour to move.
    pub actv: u64,
    /// Bitboard of the pieces of the colour that moved.
    pub idle: u64,

    // todo split actv/idle
    pub actv_pawns: u64,
    pub actv_bishops: u64,
    pub actv_knights: u64,
    pub actv_rooks: u64,
    pub actv_queens: u64,
    pub actv_king_sq: u8,

    pub idle_pawns: u64,
    pub idle_bishops: u64,
    pub idle_knights: u64,
    pub idle_rooks: u64,
    pub idle_queens: u64,
    pub idle_king_sq: u8,

    /// Move count since game begin.
    pub move_count: u16,
    /// `1` is white, `-1` is black.
    pub colour: i8,

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
}

impl Board {
    /// Returns the piece at sq, if there is one.
    pub fn get_actv_piece_at(&self, mask: u64) -> Option<Piece> {
        if self.actv & mask == 0 {
            None
        } else {
            Some(  if self.actv_pawns & mask != 0 {
                Piece::Pawn
            } else if self.actv_knights & mask != 0 {
                Piece::Knight
            } else if self.actv_bishops & mask != 0 {
                Piece::Bishop
            } else if self.actv_rooks & mask != 0 {
                Piece::Rook
            } else if self.actv_queens & mask != 0 {
                Piece::Queen
            } else { // king
                Piece::King
            })
        }
    }
    /// Returns the piece at sq, if there is one.
    pub fn get_idle_piece_at(&self, mask: u64) -> Option<Piece> {
        if self.idle & mask == 0 {
            None
        } else {
            Some(  if self.idle_pawns & mask != 0 {
                Piece::Pawn
            } else if self.idle_knights & mask != 0 {
                Piece::Knight
            } else if self.idle_bishops & mask != 0 {
                Piece::Bishop
            } else if self.idle_rooks & mask != 0 {
                Piece::Rook
            } else if self.idle_queens & mask != 0 {
                Piece::Queen
            } else { // king
                Piece::King
            })
        }
    }

    fn is_tile_cvrd_actv(&self, sq: u8, all: u64) -> bool {
        fend::knight_fend(sq) & self.actv_knights != 0
        || fend::bishop_fend(sq, all) & (self.actv_bishops | self.actv_queens) != 0
        || fend::rook_fend(sq, all) & (self.actv_rooks | self.actv_queens) != 0
        || fend::pawn_fend_idle(sq) & self.actv_pawns != 0
        || fend::king_fend(sq) & (1 << self.actv_king_sq) != 0
    }
    fn is_tile_cvrd_idle(&self, sq: u8, all: u64) -> bool {
        fend::knight_fend(sq) & self.idle_knights != 0
        || fend::bishop_fend(sq, all) & (self.idle_bishops | self.idle_queens) != 0
        || fend::rook_fend(sq, all) & (self.idle_rooks | self.idle_queens) != 0
        || fend::pawn_fend_actv(sq) & self.idle_pawns != 0
        || fend::king_fend(sq) & (1 << self.idle_king_sq) != 0
    }

    #[inline]
    pub fn is_actv_in_check(&self) -> bool {
        self.is_tile_cvrd_idle(self.actv_king_sq, self.all)
    }
    #[inline]
    pub fn is_idle_in_check(&self) -> bool {
        self.is_tile_cvrd_actv(self.idle_king_sq, self.all)
    }

    /// Checks whether a generates pseudo-legal move is legal.
    pub fn is_move_legal(&self, Move { from_sq, to_sq, piece }: Move) -> bool {
        let king_sq = if let Piece::King = piece { to_sq } else { self.actv_king_sq };
        !self.is_tile_cvrd_idle(king_sq, self.all & !(1 << from_sq) | (1 << to_sq))
    }

    #[inline]
    fn flip(&mut self) {
        self.colour = -self.colour;
        
        (self.actv_pawns, self.idle_pawns) = (self.idle_pawns.swap_bytes(), self.actv_pawns.swap_bytes());
        (self.actv_knights, self.idle_knights) = (self.idle_knights.swap_bytes(), self.actv_knights.swap_bytes());
        (self.actv_bishops, self.idle_bishops) = (self.idle_bishops.swap_bytes(), self.actv_bishops.swap_bytes());
        (self.actv_rooks, self.idle_rooks) = (self.idle_rooks.swap_bytes(), self.actv_rooks.swap_bytes());
        (self.actv_queens, self.idle_queens) = (self.idle_queens.swap_bytes(), self.actv_queens.swap_bytes());
        (self.actv_king_sq, self.idle_king_sq) = (flip_sq(self.idle_king_sq), flip_sq(self.actv_king_sq));
        (self.actv, self.idle) = (self.idle.swap_bytes(), self.actv.swap_bytes());
        (self.actv_castle_flags, self.idle_castle_flags) = (self.idle_castle_flags, self.actv_castle_flags);

        self.all = self.actv | self.idle;
        self.hash ^= zobrist::COLOUR_HASH;
    }


    /// Play a move on the board, and flip the player turn.
    /// 
    /// Note that legality is not checked.
    pub fn make(&mut self, Move { from_sq, to_sq, piece }: Move) {
        let is_actv_white = self.colour == 1;
        let from = 1u64 << from_sq;
        let to = 1u64 << to_sq;
        
        self.fifty_move_clock += 1;
        
        // handle capture
        if let Some(cap) = self.get_idle_piece_at(to) {
            // remove piece & reset 50-move rule clock
            self.idle &= !to;
            self.fifty_move_clock = 0;
            self.hash ^= zobrist::get_piece_hash_idle(!is_actv_white, cap, to_sq);
            match cap {
                Piece::Pawn => self.idle_pawns &= !to,
                Piece::Knight => self.idle_knights &= !to,
                Piece::Bishop => self.idle_bishops &= !to,
                Piece::Rook => self.idle_rooks &= !to,
                Piece::Queen => self.idle_queens &= !to,
                Piece::King => panic!("king captured!"),
            }
        } else {
            // check for en passant before assuming no capture
            if piece == Piece::Pawn && to == self.en_passant {
                let cap = to >> 8;
                self.idle_pawns &= !cap;
                self.idle &= !cap;
                self.fifty_move_clock = 0;
                self.hash ^= zobrist::get_piece_hash_idle(
                    !is_actv_white,
                    Piece::Pawn,
                    to_sq - 0o10
                );
            }
        }
        
        self.en_passant = 0; // clear between potential read and write

        match piece {
            Piece::Pawn => {
                self.actv_pawns = self.actv_pawns & !from | to;
                // reset 50 move clock
                self.fifty_move_clock = 0;
                // double-advance: flag rear tile for en passant
                if to == from << 16 {
                    // shift accounts for board flip
                    self.en_passant = from << 0o40;
                }
            }
            Piece::Knight => self.actv_knights = self.actv_knights & !from | to,
            Piece::Bishop => self.actv_bishops = self.actv_bishops & !from | to,
            Piece::Queen =>  self.actv_queens  = self.actv_queens  & !from | to,
            Piece::Rook => {
                self.actv_rooks = self.actv_rooks & !from | to;
                if from == 0o0 {
                    self.actv_castle_flags &= !CastleFlags::QUEENSIDE;
                    self.hash ^= zobrist::get_hash_qs_castle(is_actv_white);
                } else if from == 0o7 {
                    self.actv_castle_flags &= !CastleFlags::KINGSIDE;
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
            },
            Piece::King => {
                self.actv_king_sq = to_sq;
                if from_sq == to_sq - 2 { // kingside castle!
                    self.actv_rooks = self.actv_rooks & !0x80 | 0x20;
                    self.actv = self.actv & !0x80 | 0x20;
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o7);
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o5);
                } else if from_sq == to_sq + 2 { // queenside castle!
                    self.actv_rooks = self.actv_rooks & !0x1 | 0x8;
                    self.actv = self.actv & !0x1 | 0x8;
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o0);
                    self.hash ^= zobrist::get_piece_hash_actv(is_actv_white, Piece::Rook, 0o3);
                }
                if self.actv_castle_flags.contains(CastleFlags::KINGSIDE) {
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
                if self.actv_castle_flags.contains(CastleFlags::QUEENSIDE) {
                    self.hash ^= zobrist::get_hash_qs_castle(is_actv_white);
                }
                self.actv_castle_flags = CastleFlags::empty();
            }
        }
        
        // update actv
        self.actv = self.actv & !from | to;
        // update hash & handle pawn promotions
        if self.actv_pawns & from != 0 {
            self.actv_pawns &= !from;
            self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, Piece::Pawn, from_sq);
        } else {
            self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, piece, from_sq);
        }
        self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, piece, to_sq);

        // bookkeeping
        self.flip();
        self.move_count += (self.colour + 1) as u16 >> 1;
    }

    pub fn unmake(&mut self) {

    }


    /// Play a null move and flip the player turn.
    pub fn make_null(&mut self) {
        self.flip();
        self.fifty_move_clock += 1;
        self.move_count += (self.colour + 1) as u16 >> 1;
    }

    /// Check for stalemate and checkmate.
    pub fn is_mate(&self) -> Option<GameOver> {
        let mut move_table = mov::MoveSetTable::new();
        self.get_moveset_actv(&mut move_table);
        
        // if any move avoids check, no mate
        // `make` does not mutate state unless a legal move is successfully 
        // played, thus it need only be fixme todo
        for set in move_table.get_move_sets() {
            for mov in set.iter() {
                if self.is_move_legal(mov) { return None; }
            }
        }
        for set in move_table.get_prom_sets() {
            for pro in set.iter() {
                if self.is_move_legal(pro) { return None; }
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
        if self.actv_pawns | self.idle_pawns | self.actv_rooks | self.idle_rooks | self.actv_queens | self.idle_queens == 0 {
            let actv_minor_thresh = (self.actv_bishops | self.actv_knights).count_ones() <= 1;
            let idle_minor_thresh = (self.idle_bishops | self.idle_knights).count_ones() <= 1;
            
            if actv_minor_thresh && idle_minor_thresh {
                // kx v kx, where x is a minor piece or nothing
                return Some(GameOver::InsMaterial);
            } else if actv_minor_thresh || idle_minor_thresh {
                if self.actv_bishops == 0 && self.idle_bishops == 0 {
                    if (self.actv_knights == 2) ^ (self.idle_knights == 2) {
                        return Some(GameOver::InsMaterial); // knn v k
                    }
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

    
    /// Check whether the `actv` king would be kingside castling through check.
    #[inline]
    fn can_kingside_castle(&self) -> bool {
        let idle_strt = self.idle_rooks | self.idle_queens;
        let idle_diag = self.idle_bishops | self.idle_queens;
        0xF8DC00 & self.idle_knights == 0
        && fend::bishop_fend(4, self.all) & idle_diag == 0
        && fend::bishop_fend(5, self.all) & idle_diag == 0
        && fend::bishop_fend(6, self.all) & idle_diag == 0
        && fend::rook_fend(4, self.all) & idle_strt == 0
        && fend::rook_fend(5, self.all) & idle_strt == 0
        && fend::rook_fend(6, self.all) & idle_strt == 0
        && 0xF800 & self.idle_pawns == 0
        && 0xF8F8 & (1u64 << self.idle_king_sq) == 0
    }
    /// Check whether the `actv` king would be queenside castling through check.
    #[inline]
    fn can_queenside_castle(&self) -> bool {
        let idle_strt = self.idle_rooks | self.idle_queens;
        let idle_diag = self.idle_bishops | self.idle_queens;
        0x3E7700 & self.idle_knights == 0
        && fend::bishop_fend(4, self.all) & idle_diag == 0
        && fend::bishop_fend(3, self.all) & idle_diag == 0
        && fend::bishop_fend(2, self.all) & idle_diag == 0
        && fend::rook_fend(4, self.all) & idle_strt == 0
        && fend::rook_fend(3, self.all) & idle_strt == 0
        && fend::rook_fend(2, self.all) & idle_strt == 0
        && 0x3E00 & self.idle_pawns == 0
        && 0x3E3E & (1u64 << self.idle_king_sq) == 0
    }
    
    /// Computes the bitmap of 'covered' tiles by `actv`.
    pub fn calc_actv_fend(&self) -> u64 {
        let mut fend = 0;
        fend |= fend::pawns_fend_actv(self.actv_pawns);
        fend |= fend::knights_fend(self.actv_knights);
        fend |= fend::bishops_fend(self.actv_bishops, self.all);
        fend |= fend::rooks_fend(self.actv_rooks, self.all);
        fend |= fend::queens_fend(self.actv_queens, self.all);
        fend |= fend::king_fend(self.actv_king_sq);
        fend
    }
    /// Computes the bitmap of 'covered' tiles by `idle`.
    pub fn calc_idle_fend(&mut self) -> u64 {
        let mut fend = 0;
        fend |= fend::pawns_fend_idle(self.idle_pawns);
        fend |= fend::knights_fend(self.idle_knights);
        fend |= fend::bishops_fend(self.idle_bishops, self.all);
        fend |= fend::rooks_fend(self.idle_rooks, self.all);
        fend |= fend::queens_fend(self.idle_queens, self.all);
        fend |= fend::king_fend(self.idle_king_sq); 
        fend
    }
}

