//! Chess board and move string coding and conversion.

use super::Piece;


/// Converts a position from algebraic notation.
/// ### Panics:
/// Panics when `alg` byte length is not 2.
pub fn alg_pos_to_sq(alg: &str) -> Option<usize> {
    let arr = alg.as_bytes();
    assert_eq!(arr.len(), 2);
    let file = arr[0].to_ascii_lowercase();
    let rank = arr[1].to_ascii_lowercase();
    if file >= b'a' && file <= b'h' || rank >= b'1' && rank <= b'8' {
        Some((file - b'a') as usize + (rank - b'1') as usize * 8)
    } else {
        None
    }
}
/// Converts a position to algebraic notation.
/// ### Returns:
/// Valid ASCII and UTF8 bytes. File is in lowercase.
/// ### Panics:
/// Panics when `sq` is bigger than 63.
pub fn sq_to_alg_pos(sq: usize) -> String {
    assert!(sq < 64);
    let mut alg_pos = String::with_capacity(2);
    alg_pos.push(char::from_u32(sq as u32 % 8 + b'a' as u32).unwrap());
    alg_pos.push(char::from_u32(sq as u32 / 8 + b'1' as u32).unwrap());
    alg_pos
}

pub struct Move {
    pub from: usize,
    pub to: usize,
    pub promotion: Option<Piece>,
}

impl Move {
    /// Convert from pure coordinate notation.
    /// ### Panics:
    /// Panics when `coord` byte length is not 4 or 5.
    pub fn from_pure_coord(coord: &str) -> Option<Self> {
        let from = alg_pos_to_sq(&coord[0..2])?;
        let to = alg_pos_to_sq(&coord[2..4])?;
        
        match coord.len() {
            4 => Some(Self { from, to, promotion: None }),
            5 => {
                let promo = match coord.as_bytes()[5] {
                    b'q' => Piece::Queen,
                    b'n' => Piece::Knight,
                    b'r' => Piece::Rook,
                    b'b' => Piece::Bishop,
                    _ => return None,
                };

                Some(Self { from, to, promotion: Some(promo) })
            },
            _ => panic!("Invalid coord argument length."),
        }
    }

    /// Convert to pure coordinate notation.
    pub fn to_pure_coord(&self) -> String {
        match self.promotion {
            Some(promo) => {
                let mut alg_pos = String::with_capacity(4);
                alg_pos.push_str(sq_to_alg_pos(self.from).as_str());
                alg_pos.push_str(sq_to_alg_pos(self.to).as_str());
                alg_pos.push(match promo {
                    Piece::Knight => 'n',
                    Piece::Bishop => 'b',
                    Piece::Rook => 'r',
                    Piece::Queen => 'q',
                    _ => panic!("Invalid promotion piece."),
                });
                alg_pos
            },
            None => {
                let mut alg_pos = String::with_capacity(4);
                alg_pos.push_str(sq_to_alg_pos(self.from).as_str());
                alg_pos.push_str(sq_to_alg_pos(self.to).as_str());
                alg_pos
            },
        }
    }
}

impl super::Board {
    pub const START_POS_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    /* /// Validate and play `mov`.
    pub fn make_move(&mut self, mov: Move) -> Result<(), ()> {

        todo!()
    } */

    pub fn from_fen(fen: &str) -> Result<Self, ()> {
        if !fen.is_ascii() { return Err(()); }

        // decompose string
        let mut split = fen.split_ascii_whitespace();
        let pieces = split.next().ok_or(())?.trim();
        let colour = split.next().ok_or(())?.trim();
        let castle_cap = split.next().ok_or(())?.trim();
        let en_passant = split.next().ok_or(())?.trim();
        let fifty_ply_clock = split.next().ok_or(())?.trim();
        let move_count = split.next().ok_or(())?.trim();

        
        let mut board = Self {
            hash: 0,
            all: 0,
            actv: 0,
            idle: 0,
            idle_fend: 0,
            actv_fend: 0,
            pawns: 0,
            bishops: 0,
            knights: 0,
            rooks: 0,
            queens: 0,
            kings: 0,
            en_passant: 0,
            move_count: move_count.parse::<usize>().map_err(|_| ())?,
            colour: 0,
            fifty_ply_clock: fifty_ply_clock.parse::<u8>().map_err(|_| ())?,
            actv_castle_flags: super::CastleFlags::empty(),
            idle_castle_flags: super::CastleFlags::empty(),
        };

        // determine colour
        board.colour = match colour {
            "w" => 1,
            "b" => -1,
            _ => return Err(()),
        };

        // determine en passant status
        board.en_passant = if en_passant == "-" {
            0
        } else {
            if en_passant.len() == 2 {
                1 << alg_pos_to_sq(en_passant).ok_or(())?
            } else {
                return Err(());
            }
        };
        
        // create white/black references to actv/idle
        let (white, white_castle, black, black_castle) = if board.colour == 1 { (
            &mut board.actv, &mut board.actv_castle_flags,
            &mut board.idle, &mut board.idle_castle_flags,
        ) } else { (
            &mut board.idle, &mut board.idle_castle_flags,
            &mut board.actv, &mut board.actv_castle_flags,
        ) };
        
        // castling capabilities
        if castle_cap.contains('K') { *white_castle |= super::CastleFlags::KINGSIDE; }
        if castle_cap.contains('Q') { *white_castle |= super::CastleFlags::QUEENSIDE; }
        if castle_cap.contains('k') { *black_castle |= super::CastleFlags::KINGSIDE; }
        if castle_cap.contains('q') { *black_castle |= super::CastleFlags::QUEENSIDE; }

        // parse piece location data
        let mut sq = 56usize; // fen ranks are in reverse order
        for c in pieces.chars() {
            if c == '/' {
                assert!(sq % 8 == 0);
                sq -= 16;
            } else if c.is_ascii_digit() {
                sq += c.to_digit(16).unwrap() as usize;
            } else {
                if c.is_uppercase() {
                    *white |= 1 << sq;
                } else {
                    *black |= 1 << sq;
                }
                
                match c.to_lowercase().next().ok_or(())? {
                    'p' => board.pawns |= 1 << sq,
                    'n' => board.knights |= 1 << sq,
                    'b' => board.bishops |= 1 << sq,
                    'r' => board.rooks |= 1 << sq,
                    'q' => board.queens |= 1 << sq,
                    'k' => board.kings |= 1 << sq,
                    _ => return Err(()),
                }
                sq += 1;
            }
        }
        board.all = board.actv | board.idle;

        // initialize dependent data
        board.calc_idle_fend();
        board.calc_actv_fend();
        
        Ok(board)
    }

    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        let (white, white_castle, black_castle) = if self.colour == 1 { (
            self.actv, self.actv_castle_flags, self.idle_castle_flags,
        ) } else { (
            self.idle, self.idle_castle_flags, self.actv_castle_flags,
        ) };


        for rank in (0..8).rev() { // fen ranks are in reverse order
            let mut rank_str = String::new();
            let mut blank_count = 0;
            for sq in (rank * 8)..(rank * 8 + 8) {
                let mask = 1u64 << sq;

                if self.all & mask == 0 {
                    blank_count += 1;
                    continue;
                } else {
                    if blank_count > 0 {
                        rank_str.push_str(blank_count.to_string().as_str());
                        blank_count = 0;
                    }
                }

                let c;
                if self.pawns & mask != 0 {
                    c = 'p';
                } else if self.knights & mask != 0 {
                    c = 'n';
                } else if self.bishops & mask != 0 {
                    c = 'b';
                } else if self.rooks & mask != 0 {
                    c = 'r';
                } else if self.queens & mask != 0 {
                    c = 'q';
                } else {
                    c = 'k';
                }

                if white & mask != 0 {
                    rank_str.push(c.to_ascii_uppercase())
                } else {
                    rank_str.push(c)
                }
            }

            if blank_count > 0 {
                rank_str.push_str(blank_count.to_string().as_str());
            }
            if rank > 0 {
                rank_str.push('/');
            }

            fen.push_str(rank_str.as_str());
        }
        fen.push(' ');

        fen.push(if self.colour == 1 { 'w' } else { 'b' });
        fen.push(' ');

        if (white_castle | black_castle) == super::CastleFlags::empty() {
            fen.push('-');
        } else {
            if white_castle.contains(super::CastleFlags::KINGSIDE)  { fen.push('K'); }
            if white_castle.contains(super::CastleFlags::QUEENSIDE) { fen.push('Q'); }
            if black_castle.contains(super::CastleFlags::KINGSIDE)  { fen.push('k'); }
            if black_castle.contains(super::CastleFlags::QUEENSIDE) { fen.push('q'); }
        }
        fen.push(' ');

        if self.en_passant == 0 {
            fen.push('-');
        } else {
            let pos = sq_to_alg_pos(self.en_passant.trailing_zeros() as usize);
            fen.push_str(pos.as_str());
        }
        fen.push(' ');

        fen.push_str(self.fifty_ply_clock.to_string().as_str());
        fen.push(' ');

        fen.push_str(self.move_count.to_string().as_str());

        fen
    }
}

#[cfg(test)]
mod tests {
    use crate::board::Board;
    use super::*;

    #[test]
    fn test_to_from_fen() {
        let fen1 = "r1bqk1nr/pppp1ppp/2B5/2b1p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQ - 0 4";
        let fen2 = "R7/6k1/8/8/P6P/6K1/8/4r3 b - - 0 1";

        assert_eq!(Board::START_POS_FEN, crate::board::Board::from_fen(Board::START_POS_FEN).unwrap().to_fen());
        assert_eq!(fen1, crate::board::Board::from_fen(fen1).unwrap().to_fen());
        assert_eq!(fen2, crate::board::Board::from_fen(fen2).unwrap().to_fen());
    }
}

