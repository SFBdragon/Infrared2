use std::io::Read;

use once_cell::sync::Lazy;

use crate::{Move, Piece};


pub static BOOK: Lazy<Vec<OpeningEntry>> = Lazy::new(|| {
    let mut file = match std::fs::File::open("openings.dat") {
        Ok(file) => file,
        Err(_) => {
            eprintln!("No opening book file found!");
            return Vec::new();
        },
    };

    let mut vec = Vec::new();
    let mut buf = [0u8; 16];
    while let Ok(_) = file.read_exact(&mut buf) {
        let piece = match buf[14] {
            0 => Some(Piece::King),
            1 => Some(Piece::Queen),
            2 => Some(Piece::Rook),
            3 => Some(Piece::Bishop),
            4 => Some(Piece::Knight),
            5 => Some(Piece::Pawn),
            _ => None,
        };

        if buf[15] != 0xff || piece.is_none() || buf[13] >= 64 || buf[12] >= 64 {
            eprintln!("Invalid opening book file!");
            vec = Vec::new();
            break;
        }

        let mut hash_buf = [0u8; 8];
        for i in 0..8  { hash_buf[i] = buf[i]; }
        let mut freq_buf = [0u8; 4];
        for i in 8..12 { freq_buf[i - 8] = buf[i]; }
        
        vec.push(OpeningEntry {
            hash: u64::from_le_bytes(hash_buf),
            freq: u32::from_le_bytes(freq_buf),
            mov: Move::new(buf[12], buf[13], piece.unwrap()),
        });
    }

    vec
});

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpeningEntry {
    /// Hash of entry position.
    pub hash: u64,
    /// Relative frequency of play.
    pub freq: u32,
    /// Move to play at this position.
    pub mov: Move,
}


pub fn query_book(hash: u64) -> Option<&'static [OpeningEntry]> {
    if BOOK.len() == 0 { return None; }

    if let Ok(index) = BOOK.binary_search_by_key(&hash, |oe| oe.hash) {
        let mut start = index;
        if start != 0 { while BOOK[start - 1].hash == hash { start -= 1; if start == 0 { break; } } }
        let mut end = index + 1;
        if end != BOOK.len() { while BOOK[end].hash == hash { end += 1; if end == BOOK.len() { break; } } }

        Some(BOOK.get(start..end).unwrap())
    } else {
        None
    }
}

pub fn query_book_best(hash: u64) -> Option<Move> {
    query_book(hash).map(|q| q.iter().max_by_key(|e| e.freq).unwrap().mov)
}


#[cfg(test)]
mod tests {
    #[test]
    pub fn test_query_startpos() {
        assert_eq!(
            crate::Move::new(0o14, 0o34, crate::Piece::Pawn), // good ol' kings pawn opening
            super::query_book_best(crate::Board::default().hash).unwrap()
        );

        /* let mut b = crate::Board::default();
        b.make(crate::Move::new(0o14, 0o34, crate::Piece::Pawn));
        b.make(crate::Move::new(0o14, 0o34, crate::Piece::Pawn)); */
        panic!("{:#?}", super::query_book_best(0xb1926f79b1bd3788/* b.hash */));
    }
}

