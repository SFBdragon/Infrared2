use std::hash::Hasher;




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

// 64 * 7 * 2 (pieces) + 8 (en_passant) + 1 (colour)

/* pub const COLOUR_HASH_INDEX: usize = todo!();
pub const EP_FILE_HASHES_INDEX: usize = todo!();
pub const PIECE_COLOUR_HASHES_INDEX: usize = todo!();


static mut LFSR: u16 = 0xBEDDu16;
fn xorshift_lfsr() -> u16 {
    unsafe {
        LFSR ^= LFSR >> 7;
        LFSR ^= LFSR << 9;
        LFSR ^= LFSR >> 13;

        return LFSR;
    }
} */
