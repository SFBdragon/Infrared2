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
            let $sq = t.trailing_zeros() as usize;
            { $blk }
            t &= t - 1; // blsr usually isn't any faster
        }
    }
}
/// Loop through each set bit from least to most significant.
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
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameOver {
    Checkmate,
    Stalemate,
    ThreeMoveRep,
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
    /// Bitboard of the squares 'covered' by `idle` pieces.
    pub actv_fend: u64,
    /// Bitboard of the squares 'covered' by `idle` pieces.
    pub idle_fend: u64,

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
    /// Count plies to 50 from last capture or pawn-advance.
    pub fifty_ply_clock: u8,
    /// `next`'s castling capabilities.
    pub actv_castle_flags: CastleFlags,
    /// `prev`'s castling capabilities.
    pub idle_castle_flags: CastleFlags,
}

impl Board {
    /// Initializes the `actv_fend` field.
    pub fn calc_actv_fend(&mut self) {
        let mut fend = 0;
        fend |= fend::get_pawns_fend_actv(self.pawns & self.actv);
        fend |= fend::get_knights_fend(self.knights & self.actv);
        fend |= fend::get_bishops_fend(self.bishops & self.actv, self.all);
        fend |= fend::get_rooks_fend(self.rooks & self.actv, self.all);
        fend |= fend::get_queens_fend(self.queens & self.actv, self.all);
        fend |= fend::get_king_fend((self.kings & self.actv).trailing_zeros() as usize);
        self.actv_fend = fend;
    }

    /// Initializes the `idle_fend` field.
    pub fn calc_idle_fend(&mut self) {
        let mut fend = 0;
        fend |= fend::get_pawns_fend_idle(self.pawns & self.idle);
        fend |= fend::get_knights_fend(self.knights & self.idle);
        fend |= fend::get_bishops_fend(self.bishops & self.idle, self.all);
        fend |= fend::get_rooks_fend(self.rooks & self.idle, self.all);
        fend |= fend::get_queens_fend(self.queens & self.idle, self.all);
        fend |= fend::get_king_fend((self.kings & self.idle).trailing_zeros() as usize); 
        self.idle_fend = fend;
    }

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
    pub fn make(&mut self, from: u64, to: u64, piece: Piece) -> Result<(), ()> {
        self.fifty_ply_clock += 1;

        if let Some(cap) = self.get_piece_at(to) {
            // remove piece & reset 50-move rule clock
            self.idle &= !to;
            self.fifty_ply_clock = 0;
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
                self.fifty_ply_clock = 0;
            }
        }
        
        self.actv = self.actv & !from | to;
        self.en_passant = 0;

        match piece {
            Piece::Pawn => {
                self.pawns = self.pawns & !from | to;
                // reset 50 move clock
                self.fifty_ply_clock = 0;
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
                }
                if from == 0x80 {
                    self.actv_castle_flags &= !CastleFlags::KINGSIDE;
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
            }
        }

        // handle pawn promotions
        self.pawns &= !from;

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

        self.calc_actv_fend();

        if self.actv_fend & self.kings & self.idle != 0 {
            // idle is in check (illegal)
            return Err(());
        }

        self.calc_idle_fend();

        Ok(())
    }

    #[inline]
    pub fn is_actv_in_check(&self) -> bool {
        self.kings & self.actv & self.idle_fend != 0
    }
    
    /// Check if the game is over.
    /// 
    /// Note: does not check 3 position repetition.
    pub fn is_game_over(&self) -> Option<GameOver> {
        // 50 ply rule
        if self.fifty_ply_clock >= 50 {
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

        // stale- & checkmate
        let mut move_table = mov::MoveSetTable::new();
        self.get_moveset_actv(&mut move_table);
        
        // if any move avoids check, no mate
        for set in move_table.get_move_sets() {
            for (from, to, piece) in set.iter() {
                let mut board = self.clone();
                if let Ok(_) = board.make(from, to, piece) {
                    return None;
                }
            }
        }
        for set in move_table.get_prom_sets() {
            for (from, to, piece) in set.iter() {
                let mut board = self.clone();
                if let Ok(_) = board.make(from, to, piece) {
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
}



