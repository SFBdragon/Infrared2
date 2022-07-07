//! Chess board and move string coding and conversion.

use super::{Piece, Board};


/// Converts a position from algebraic notation.
/// ### Panics:
/// Panics when `alg` byte length is not 2.
fn alg_pos_to_sq(alg: &str) -> Option<u8> {
    let arr = alg.as_bytes();
    assert_eq!(arr.len(), 2);
    let file = arr[0].to_ascii_lowercase();
    let rank = arr[1].to_ascii_lowercase();
    if file >= b'a' && file <= b'h' || rank >= b'1' && rank <= b'8' {
        Some((file - b'a') + (rank - b'1') * 8)
    } else {
        None
    }
}
/// Converts a position to algebraic notation.
/// ### Returns:
/// Valid ASCII and UTF8 bytes. File is in lowercase.
/// ### Panics:
/// Panics when `sq` is bigger than 63.
fn sq_to_alg_pos(sq: u8) -> String {
    assert!(sq < 64);
    let mut alg_pos = String::with_capacity(2);
    alg_pos.push(char::from_u32((sq % 8 + b'a') as u32).unwrap());
    alg_pos.push(char::from_u32((sq / 8 + b'1') as u32).unwrap());
    alg_pos
}

impl Default for Board {
    fn default() -> Self {
        Self::from_fen(Self::START_POS_FEN).unwrap()
    }
}

impl Board {
    pub const START_POS_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    
    /// Deserialize position from FEN string.
    /// ### Returns:
    /// * `Ok(Board)` on successful parse.
    /// * `Err(())` upon either parse error or illegal position (`idle` in check).
    pub fn from_fen(fen: &str) -> Result<Self, &'static str> {
        if !fen.is_ascii() { return Err("FEN must be ASCII"); }

        // decompose string
        let mut split = fen.split_ascii_whitespace();
        let pieces = split.next().ok_or("not enough substrings")?;
        let colour = split.next().ok_or("not enough substrings")?;
        let castle_cap = split.next().ok_or("not enough substrings")?;
        let en_passant = split.next().ok_or("not enough substrings")?;
        let fifty_move_clock = split.next().ok_or("not enough substrings")?;
        let move_count = split.next().ok_or("not enough substrings")?;

        // init board, assume white is actv, this is fixed later
        let mut board = Self {
            hash: 0,
            all: 0,
            actv: 0,
            idle: 0,
            pawns: 0,
            bishops: 0,
            knights: 0,
            rooks: 0,
            queens: 0,
            actv_king_sq: 0,
            idle_king_sq: 0,
            en_passant: 0,
            move_count: move_count.parse::<u16>().map_err(|_| "Move counter parse failed")?,
            colour: 1,
            fifty_move_clock: fifty_move_clock.parse::<u16>().map_err(|_| "Fifty move clock parse failed")?,
            actv_castle_rights: super::CastleRights::empty(),
            idle_castle_rights: super::CastleRights::empty(),
        };

        // determine en passant status
        board.en_passant = if en_passant == "-" {
            0
        } else {
            if en_passant.len() == 2 {
                1 << alg_pos_to_sq(en_passant).ok_or("En Passant notation parse failed")?
            } else {
                return Err("En Passant FEN section contains unexpected value");
            }
        };
        
        // castling capabilities
        if castle_cap.contains('K') { board.actv_castle_rights |= super::CastleRights::KINGSIDE; }
        if castle_cap.contains('Q') { board.actv_castle_rights |= super::CastleRights::QUEENSIDE; }
        if castle_cap.contains('k') { board.idle_castle_rights |= super::CastleRights::KINGSIDE; }
        if castle_cap.contains('q') { board.idle_castle_rights |= super::CastleRights::QUEENSIDE; }

        // parse piece location data
        let mut sq = 56usize; // fen ranks are in reverse order
        for c in pieces.chars() {
            if c == '/' {
                assert!(sq % 8 == 0);
                sq -= 16;
            } else if c.is_ascii_digit() {
                sq += c.to_digit(16).unwrap() as usize;
            } else {
                if c.is_ascii_uppercase() {
                    board.actv |= 1 << sq;
                } else {
                    board.idle |= 1 << sq;
                }
                
                match c.to_lowercase().next().ok_or("Invalid piece data")? {
                    'p' => board.pawns |= 1 << sq,
                    'n' => board.knights |= 1 << sq,
                    'b' => board.bishops |= 1 << sq,
                    'r' => board.rooks |= 1 << sq,
                    'q' => board.queens |= 1 << sq,
                    'k' => if c.is_ascii_uppercase() {
                        board.actv_king_sq = sq as u8;
                    } else {
                        board.idle_king_sq = sq as u8;
                    },
                    _ => return Err("Invalid piece letter"),
                }
                sq += 1;
            }
        }

        board.all = board.actv | board.idle;

        // board init up until this point assumes white is actv
        // if this is wrong, flip the board
        match colour {
            "w" => (),
            "b" => board.flip(),
            _ => return Err("Invalid colour value"),
        };

        // once everything is said an done, calculate the hash
        board.hash = board.calc_hash();
        // then validate the position
        board.validate().map(|_| board)
    }

    /// Serialize position to FEN string.
    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        // ensure actv is white and idle is black
        let mut board = self.clone();
        if board.colour == -1 { board.flip(); }

        for rank in (0..8).rev() { // fen ranks are in reverse order
            let mut rank_str = String::new();
            let mut blank_count = 0;
            for sq in (rank * 8)..(rank * 8 + 8) {
                let mask = 1u64 << sq;

                if board.all & mask == 0 {
                    blank_count += 1;
                    continue;
                } else {
                    if blank_count > 0 {
                        rank_str.push_str(blank_count.to_string().as_str());
                        blank_count = 0;
                    }
                }

                let c = match board.get_piece_at(mask).unwrap() {
                    Piece::King =>   'k',
                    Piece::Queen =>  'q',
                    Piece::Rook =>   'r',
                    Piece::Bishop => 'b',
                    Piece::Knight => 'n',
                    Piece::Pawn =>   'p',
                };

                if board.actv & mask != 0 {
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

        if (board.actv_castle_rights | board.idle_castle_rights) == super::CastleRights::empty() {
            fen.push('-');
        } else {
            if board.actv_castle_rights.contains(super::CastleRights::KINGSIDE)  { fen.push('K'); }
            if board.actv_castle_rights.contains(super::CastleRights::QUEENSIDE) { fen.push('Q'); }
            if board.idle_castle_rights.contains(super::CastleRights::KINGSIDE)  { fen.push('k'); }
            if board.idle_castle_rights.contains(super::CastleRights::QUEENSIDE) { fen.push('q'); }
        }
        fen.push(' ');

        if board.en_passant == 0 {
            fen.push('-');
        } else {
            let pos = sq_to_alg_pos(board.en_passant.trailing_zeros() as u8);
            fen.push_str(pos.as_str());
        }
        fen.push(' ');

        fen.push_str(self.fifty_move_clock.to_string().as_str());
        fen.push(' ');

        fen.push_str(self.move_count.to_string().as_str());

        fen
    }
}

#[cfg(test)]
mod tests {
    use crate::board::Board;

    #[test]
    fn test_to_from_fen() {
        let fen1 = "r1bqk1nr/pppp1ppp/2B5/2b1p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQ - 0 4";
        let fen2 = "R7/6k1/8/8/P6P/6K1/8/4r3 b - - 0 1";

        assert_eq!(Board::START_POS_FEN, crate::board::Board::from_fen(Board::START_POS_FEN).unwrap().to_fen());
        assert_eq!(fen1, crate::board::Board::from_fen(fen1).unwrap().to_fen());
        assert_eq!(fen2, crate::board::Board::from_fen(fen2).unwrap().to_fen());
    }
}

