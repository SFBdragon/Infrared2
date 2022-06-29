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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Board {

    /// Bitboard of all pieces.
    pub all: u64,
    /// Bitboard of the pieces of the colour to move.
    pub actv: u64,
    /// Bitboard of the pieces of the colour that moved.
    pub idle: u64,

    // todo split actv/idle
    pub pawns: u64,
    pub bishops: u64,
    pub knights: u64,
    pub rooks: u64,
    pub queens: u64,
    pub kings: u64,

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


#[derive(Debug, Clone, Copy)]
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

    fn is_tile_cvrd_actv(&self, sq: u8, all: u64) -> bool {
        fend::knight_fend(sq) & self.knights & self.actv != 0
        || fend::bishop_fend(sq, all) & (self.bishops | self.queens) & self.actv != 0
        || fend::rook_fend(sq, all) & (self.rooks | self.queens) & self.actv != 0
        || fend::pawn_fend_idle(sq) & self.pawns & self.actv != 0
        || fend::king_fend(sq) & self.kings & self.actv != 0
    }
    fn is_tile_cvrd_idle(&self, sq: u8, all: u64) -> bool {
        fend::knight_fend(sq) & self.knights & self.idle != 0
        || fend::bishop_fend(sq, all) & (self.bishops | self.queens) & self.idle != 0
        || fend::rook_fend(sq, all) & (self.rooks | self.queens) & self.idle != 0
        || fend::pawn_fend_actv(sq) & self.pawns & self.idle != 0
        || fend::king_fend(sq) & self.kings & self.idle != 0
    }

    #[inline]
    pub fn is_actv_in_check(&self) -> bool {
        self.is_tile_cvrd_idle(self.actv_king_sq(), self.all)
    }
    #[inline]
    pub fn is_idle_in_check(&self) -> bool {
        self.is_tile_cvrd_actv(self.idle_king_sq(), self.all)
    }

    /// Checks whether a generates pseudo-legal move is legal.
    pub fn is_move_legal(&self, Move { from_sq, to_sq, piece }: Move) -> bool {
        let king_sq = if let Piece::King = piece { to_sq } else { self.actv_king_sq() };
        !self.is_tile_cvrd_idle(king_sq, self.all & !(1 << from_sq) | (1 << to_sq))
    }

    #[inline]
    fn flip(&mut self) {
        self.colour = -self.colour;
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
    }


    /// Play a move on the board, and flip the player turn.
    /// 
    /// Note that legality is not checked.
    pub fn make(&mut self, Move { from_sq, to_sq, piece }: Move) -> Unmake {
        let is_actv_white = self.colour == 1;
        let from = 1u64 << from_sq;
        let to = 1u64 << to_sq;

        let mut unmake = Unmake {
            pawn_move: false,
            capture: None,
            en_passant: self.en_passant,
            hash: self.hash,
            fifty_move_clock: self.fifty_move_clock,
            actv_castle_flags: self.actv_castle_flags,
            idle_castle_flags: self.idle_castle_flags,
        };
        
        self.fifty_move_clock += 1;
        
        // handle capture
        if let Some(cap) = self.get_piece_at(to) {
            // remove piece & reset 50-move rule clock
            self.idle &= !to;
            self.fifty_move_clock = 0;
            self.hash ^= zobrist::get_piece_hash_idle(!is_actv_white, cap, to_sq);
            unmake.capture = Some(Capture::Normal(cap));
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
                unmake.capture = Some(Capture::EnPassant);
            }
        }
        
        self.en_passant = 0; // clear between potential read and write

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
            }
            Piece::Knight => self.knights = self.knights & !from | to,
            Piece::Bishop => self.bishops = self.bishops & !from | to,
            Piece::Queen =>  self.queens = self.queens & !from | to,
            Piece::Rook => {
                self.rooks = self.rooks & !from | to;
                if from == 0o0 {
                    self.actv_castle_flags &= !CastleFlags::QUEENSIDE;
                    self.hash ^= zobrist::get_hash_qs_castle(is_actv_white);
                } else if from == 0o7 {
                    self.actv_castle_flags &= !CastleFlags::KINGSIDE;
                    self.hash ^= zobrist::get_hash_ks_castle(is_actv_white);
                }
            },
            Piece::King => {
                self.kings = self.kings & !from | to;
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
        if self.pawns & from != 0 {
            unmake.pawn_move = true;
            self.pawns &= !from;
            self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, Piece::Pawn, from_sq);
        } else {
            self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, piece, from_sq);
        }
        self.hash ^= zobrist::get_piece_hash_idle(is_actv_white, piece, to_sq);

        // bookkeeping
        self.flip();
        self.move_count += (self.colour + 1) as u16 >> 1;

        unmake
    }

    pub fn unmake(&mut self, Move { from_sq, to_sq, piece }: Move, unmake: &Unmake) {
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

        
        match piece {
            Piece::Pawn =>   self.pawns = self.pawns & !to | from,
            Piece::Knight => self.knights = self.knights & !to | from,
            Piece::Bishop => self.bishops = self.bishops & !to | from,
            Piece::Queen =>  self.queens = self.queens & !to | from,
            Piece::Rook =>   self.rooks = self.rooks & !to | from,
            Piece::King => {
                self.kings = self.kings & !to | from;
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
                    // remove piece & reset 50-move rule clock
                    self.idle |= to;
                    match piece {
                        Piece::Pawn => self.pawns |= to,
                        Piece::Knight => self.knights |= to,
                        Piece::Bishop => self.bishops |= to,
                        Piece::Rook => self.rooks |= to,
                        Piece::Queen => self.queens |= to,
                        Piece::King => panic!("king captured!"),
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

    
    /// Check whether the `actv` king would be kingside castling through check.
    fn can_kingside_castle(&self) -> bool {
        let idle_strt = (self.rooks | self.queens) & self.idle;
        let idle_diag = (self.bishops | self.queens) & self.idle;
        self.idle & self.knights & 0xF8DC00 == 0
        && fend::bishop_fend(4, self.all) & idle_diag == 0
        && fend::bishop_fend(5, self.all) & idle_diag == 0
        && fend::bishop_fend(6, self.all) & idle_diag == 0
        && fend::rook_fend(4, self.all) & idle_strt == 0
        && fend::rook_fend(5, self.all) & idle_strt == 0
        && fend::rook_fend(6, self.all) & idle_strt == 0
        && 0xF800 & self.idle & self.pawns == 0
        && 0xF8F8 & self.idle & self.kings == 0
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
        && 0x3E3E & self.idle & self.kings == 0
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



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
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
    }
}
