//! Zobrist hashing for transposition tables & 3 position repetition.

use std::collections::HashMap;

use crate::{for_sq, Side, Sq, Piece, Board};


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
    let mut state = rkiss_init(0x71441EA136508DD9);
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

pub const fn get_piece_hash_actv(side: Side, piece: Piece, mut sq: Sq) -> u64 {
    let offset = match side {
        Side::White => 0,
        Side::Black => { sq = sq.flip(); PIECE_OFFSET },
    };

    PRNS[offset + piece as usize * 64 + sq.us()]
}
pub const fn get_piece_hash_idle(side: Side, piece: Piece, mut sq: Sq) -> u64 {
    let offset = match side {
        Side::White => { sq = sq.flip(); 0 },
        Side::Black => PIECE_OFFSET,
    };

    PRNS[offset + piece as usize * 64 + sq.us()]
}
pub const fn get_hash_ks_castle(side: Side) -> u64 {
    match side {
        Side::White => PRNS[CASTLE_OFFSET + 0],
        Side::Black => PRNS[CASTLE_OFFSET + 2],
    }
}
pub const fn get_hash_qs_castle(side: Side) -> u64 {
    match side {
        Side::White => PRNS[CASTLE_OFFSET + 1],
        Side::Black => PRNS[CASTLE_OFFSET + 3],
    }
}
pub const fn get_hash_en_passant(en_passant: u64) -> u64 {
    if en_passant == 0 { return 0; }
    PRNS[EP_FILE_OFFSET + (en_passant.trailing_zeros() % 8) as usize]
}
pub const COLOUR_HASH: u64 = PRNS[COLOUR_OFFSET];



impl Board {
    pub fn calc_hash(&self) -> u64 {
        let mut hash = 0;
        
        if self.side.is_white() {
            hash ^= COLOUR_HASH;
        }

        hash ^= get_hash_en_passant(self.en_passant);

        if self.actv_castle_rights.kingside()  { hash ^= get_hash_ks_castle(self.side); }
        if self.actv_castle_rights.queenside() { hash ^= get_hash_qs_castle(self.side); }
        if self.idle_castle_rights.kingside()  { hash ^= get_hash_ks_castle(!self.side); }
        if self.idle_castle_rights.queenside() { hash ^= get_hash_qs_castle(!self.side); }

        for_sq!(sq in self.pawns & self.actv => { hash ^= get_piece_hash_actv(self.side, Piece::Pawn, sq); });
        for_sq!(sq in self.pawns & self.idle => { hash ^= get_piece_hash_idle(!self.side, Piece::Pawn, sq); });
        for_sq!(sq in self.knights & self.actv => { hash ^= get_piece_hash_actv(self.side, Piece::Knight, sq); });
        for_sq!(sq in self.knights & self.idle => { hash ^= get_piece_hash_idle(!self.side, Piece::Knight, sq); });
        for_sq!(sq in self.bishops & self.actv => { hash ^= get_piece_hash_actv(self.side, Piece::Bishop, sq); });
        for_sq!(sq in self.bishops & self.idle => { hash ^= get_piece_hash_idle(!self.side, Piece::Bishop, sq); });
        for_sq!(sq in self.rooks & self.actv => { hash ^= get_piece_hash_actv(self.side, Piece::Rook, sq); });
        for_sq!(sq in self.rooks & self.idle => { hash ^= get_piece_hash_idle(!self.side, Piece::Rook, sq); });
        for_sq!(sq in self.queens & self.actv => { hash ^= get_piece_hash_actv(self.side, Piece::Queen, sq); });
        for_sq!(sq in self.queens & self.idle => { hash ^= get_piece_hash_idle(!self.side, Piece::Queen, sq); });
        hash ^= get_piece_hash_actv(self.side, Piece::King, self.actv_king);
        hash ^= get_piece_hash_idle(!self.side, Piece::King, self.idle_king);

        hash
    }
}

#[derive(Debug, Clone, Copy)]
pub struct U64IdentHasher {
    hash: u64,
}
impl std::hash::Hasher for U64IdentHasher {
    fn finish(&self) -> u64 {
        self.hash
    }

    fn write(&mut self, _bytes: &[u8]) {
        panic!("not a generic hasher!");
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct U64IdentHashBuilder;
impl std::hash::BuildHasher for U64IdentHashBuilder {
    type Hasher = U64IdentHasher;

    fn build_hasher(&self) -> Self::Hasher {
        U64IdentHasher { hash: 0 }
    }
}

/// A type to store previous positions' hashes within an exploration.
pub struct PosHashNode<'a> {
    pub hash: u64,
    pub prev: Option<&'a PosHashNode<'a>>,
    pub prev_prev: Option<&'a PosHashNode<'a>>,
}
impl<'a> PosHashNode<'a> {
    pub fn new(hash: u64, prev: Option<&'a PosHashNode>) -> Self {
        Self { hash, prev, prev_prev: prev.and_then(|p| p.prev) }
    }
}

/// A type to store previous positions' hashes across a game.
/// 
/// With the map's value indicating the number of times a position has occured.
pub type PosHashMap = HashMap<u64, u8, U64IdentHashBuilder>;

/// Check if a position exists within `map` at least two times.
pub fn threefold_repetition(map: &PosHashMap, hash: u64) -> bool {
    map.get(&hash).map_or(false, |&count| count >= 2)
}
