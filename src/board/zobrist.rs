//! Zobrist hashing for transposition tables & 3 position repetition.

use std::hash::Hasher;

use crate::for_sq;

use super::{CastleFlags, Piece, Board};




pub struct U64IdentHasher {
    hash: u64,
}
impl Hasher for U64IdentHasher {
    fn finish(&self) -> u64 {
        self.hash
    }
    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    fn write(&mut self, _bytes: &[u8]) {
        panic!("Hasher intended for u64 only.");
    }
}


/// https://www.chessprogramming.org/Bob_Jenkins#RKISS - oxidised
const fn rkiss(mut state: [u64; 4]) -> [u64; 4] {
    let mut i = 0;
    while i < 10 {
        let t = state[0].wrapping_sub(state[1].rotate_left(7));
        state[0] = state[1] ^ state[2].rotate_left(13);
        state[1] = state[2].wrapping_add(state[3].rotate_left(37));
        state[2] = state[3].wrapping_add(t);
        state[3] = t.wrapping_add(state[0]);
        i += 1;
    }
    state
}
/// https://www.chessprogramming.org/Bob_Jenkins#RKISS - oxidised
const fn rkiss_init(seed: u64) -> [u64; 4] {
    let mut state = [seed; 4];
    state[0] = 0xf1ea5eed;
    
    let mut i = 0;
    while i < 5 {
        state = rkiss(state);
        i += 1;
    }

    state
}

/// Pseudo Random Numbers
const PRNS: [u64; 781] = {
    let mut numbers = [0u64; 781];
    let mut state = rkiss_init(0xa9087b6a09b67);
    let mut i = 0;
    while i < numbers.len() {
        state = rkiss(state);
        numbers[i] = state[3];
        i += 1;
    }
    numbers
};

const PIECE_OFFSET: usize = 6 * 64;
const CASTLE_OFFSET: usize = 12 * 64;
const EP_FILE_OFFSET: usize = 12 * 64 + 4;
const COLOUR_OFFSET: usize = 12 * 64 + 4 + 8;

pub const fn get_piece_hash_actv(colour: bool, piece: Piece, mut sq: u8) -> u64 {
    let offset = if colour { 0 } else { sq = super::flip_sq(sq); PIECE_OFFSET };
    PRNS[offset + piece as usize * 64 + sq as usize]
}
pub const fn get_piece_hash_idle(colour: bool, piece: Piece, mut sq: u8) -> u64 {
    let offset = if colour { sq = super::flip_sq(sq); 0 } else { PIECE_OFFSET };
    PRNS[offset + piece as usize * 64 + sq as usize]
}
pub const fn get_hash_ks_castle(colour: bool) -> u64 {
    if colour {
        PRNS[CASTLE_OFFSET + 0]
    } else {
        PRNS[CASTLE_OFFSET + 2]
    }
}
pub const fn get_hash_qs_castle(colour: bool) -> u64 {
    if colour {
        PRNS[CASTLE_OFFSET + 1]
    } else {
        PRNS[CASTLE_OFFSET + 3]
    }
}
pub const fn get_hash_en_passant(en_passant: u64) -> u64 {
    if en_passant == 0 { return 0; }
    PRNS[EP_FILE_OFFSET + (en_passant % 8) as usize]
}
pub const COLOUR_HASH: u64 = PRNS[COLOUR_OFFSET];



impl Board {
    pub fn init_hash(&mut self) {
        let mut hash = 0;
        let is_actv_white = self.colour == 1;
        
        if is_actv_white {
            hash ^= COLOUR_HASH;
        }

        hash ^= get_hash_en_passant(self.en_passant);

        if self.actv_castle_flags.contains(CastleFlags::KINGSIDE) {
            hash ^= get_hash_ks_castle(is_actv_white);
        }
        if self.actv_castle_flags.contains(CastleFlags::QUEENSIDE) {
            hash ^= get_hash_qs_castle(is_actv_white);
        }
        if self.idle_castle_flags.contains(CastleFlags::KINGSIDE) {
            hash ^= get_hash_ks_castle(!is_actv_white);
        }
        if self.idle_castle_flags.contains(CastleFlags::QUEENSIDE) {
            hash ^= get_hash_qs_castle(!is_actv_white);
        }

        for_sq!(sq in self.actv_pawns => { hash ^= get_piece_hash_actv(is_actv_white, Piece::Pawn, sq) });
        for_sq!(sq in self.idle_pawns => { hash ^= get_piece_hash_idle(!is_actv_white, Piece::Pawn, sq) });
        for_sq!(sq in self.actv_knights => { hash ^= get_piece_hash_actv(is_actv_white, Piece::Knight, sq) });
        for_sq!(sq in self.idle_knights => { hash ^= get_piece_hash_idle(!is_actv_white, Piece::Knight, sq) });
        for_sq!(sq in self.actv_bishops => { hash ^= get_piece_hash_actv(is_actv_white, Piece::Bishop, sq) });
        for_sq!(sq in self.idle_bishops => { hash ^= get_piece_hash_idle(!is_actv_white, Piece::Bishop, sq) });
        for_sq!(sq in self.actv_rooks => { hash ^= get_piece_hash_actv(is_actv_white, Piece::Rook, sq) });
        for_sq!(sq in self.idle_rooks => { hash ^= get_piece_hash_idle(!is_actv_white, Piece::Rook, sq) });
        for_sq!(sq in self.actv_queens => { hash ^= get_piece_hash_actv(is_actv_white, Piece::Queen, sq) });
        for_sq!(sq in self.idle_queens => { hash ^= get_piece_hash_idle(!is_actv_white, Piece::Queen, sq) });
        hash ^= get_piece_hash_actv(is_actv_white, Piece::King, self.actv_king_sq);
        hash ^= get_piece_hash_idle(!is_actv_white, Piece::King, self.idle_king_sq);

        self.hash = hash;
    }
}
