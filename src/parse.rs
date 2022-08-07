//! Chess board, square, piece, and move string coding and conversion.

use std::collections::VecDeque;

use crate::{Sq, Board, Piece, Side, Move, GameOver, CastleRights};


impl Sq {
    /// Converts from algebraic notation.
    pub fn from_alg(alg: &str) -> Option<Self> {
        let mut chars = alg.trim().chars();
        let file = chars.next()?.to_ascii_lowercase();
        let rank = chars.next()?;

        if matches!(file, 'a'..='h') && matches!(rank, '1'..='8') {
            Some(Self::file_rank(file as u8 - b'a', rank as u8 - b'1'))
        } else {
            None
        }
    }

    /// Converts to algebraic notation. File is in lowercase.
    pub fn to_alg(self) -> String {
        let mut alg_pos = String::with_capacity(2);
        alg_pos.push(char::from_u32((self.file() + b'a') as u32).unwrap());
        alg_pos.push(char::from_u32((self.rank() + b'1') as u32).unwrap());
        alg_pos
    }
}


impl Piece {
    /// Convert from algebraic piece characters.
    pub fn from_char(ch: char) -> Option<Piece> {
        match ch {
            'K' => Some(Piece::King),
            'Q' => Some(Piece::Queen),
            'R' => Some(Piece::Rook),
            'B' => Some(Piece::Bishop),
            'N' => Some(Piece::Knight),
            'P' => Some(Piece::Pawn),
            _   => None,
        }
    }
    
    /// Convert from algebraic piece promotion characters. 
    pub fn from_char_prom(ch: char) -> Option<Piece> {
        match ch {
            'Q' => Some(Piece::Queen),
            'R' => Some(Piece::Rook),
            'B' => Some(Piece::Bishop),
            'N' => Some(Piece::Knight),
            _   => None,
        }
    }
    
    /// Convert to uppercase algebraic piece characters.
    pub fn to_char(self) -> char {
        match self {
            Piece::King =>   'K',
            Piece::Queen =>  'Q',
            Piece::Rook =>   'R',
            Piece::Bishop => 'B',
            Piece::Knight => 'N',
            Piece::Pawn =>   'P',
        }
    }
}


impl Move {
    /// Convert from Standard (or Long) Algebraic Notation.
    pub fn from_san(san: &str, board: &Board) -> Option<Self> {
        let san = san.trim_end().trim_end_matches("e.p.").trim();
    
        // detect castle case
        match san {
            "O-O" => return board.is_valid(Move::KS_CASTLE).then_some(Move::KS_CASTLE),
            "O-O-O" => return board.is_valid(Move::QS_CASTLE).then_some(Move::QS_CASTLE),
            _ => (),
        }
    
        let mut chars: VecDeque<_> = san.chars().collect();
    
        // extract piece/role information
        let piece = Piece::from_char(*chars.front()?)
            .map_or(Piece::Pawn, |p| { chars.pop_front(); p });
    
        // extract sqaure and promotion information
        let mut first_file = None;
        let mut secnd_file = None;
        let mut first_rank = None;
        let mut secnd_rank = None;
        let mut promotion = None;
    
        while let Some(c) = chars.pop_front() {
            match c {
                'a'..='h' => {
                    if first_file.is_none() { first_file = Some(c as u8 - b'a'); }
                    else if secnd_file.is_none() { secnd_file = Some(c as u8 - b'a'); }
                    else { return None; }
                }
                '1'..='8' => {
                    if first_rank.is_none() { first_rank = Some(c as u8 - b'1'); }
                    else if secnd_rank.is_none() { secnd_rank = Some(c as u8 - b'1'); }
                    else { return None; }
                }
                '=' => {
                    match chars.pop_front() {
                        Some(ch) => promotion = Some(Piece::from_char_prom(ch)?),
                        None => return None,
                    }
                }
                other => if let Some(piece) = Piece::from_char_prom(other) {
                    promotion = Some(piece);
                }, 
                // ignore x / + / #
            }
        }
    
        // organise square information
        let mut to_index = 0;
        let mut from_file = None;
        let mut from_rank = None;
        
        if let Some(sf) = secnd_file { to_index += sf; from_file = first_file; }
        else if let Some(ff) = first_file { to_index += ff; }
        else { return None; }
        if let Some(sr) = secnd_rank { to_index += sr * 0o10; from_rank = first_rank; }
        else if let Some(fr) = first_rank { to_index += fr * 0o10; }
        else { return None; }

        let to = Sq::new(to_index).cflip(board.side);
    
        if board.side == Side::Black {
            from_rank = from_rank.map(|fr| 7 - fr);
        }
    
        // test if a legal move matches the criteria (or detect ambiguity)
        let mut decode = None;
        board.for_role_mov(piece, |mov| {
            if to != mov.to {
                return false;
            }
            if let Some(from_file) = from_file {
                if from_file != mov.from.file() { return false; }
            }
            if let Some(from_rank) = from_rank {
                if from_rank != mov.from.rank() { return false; }
            }
            if let Some(prom) = promotion {
                if piece != Piece::Pawn || mov.piece != prom {
                    return false;
                }
            }
    
            // detected move ambiguity
            decode = decode.map_or(Some(mov), |_| None);
            decode.is_none()
        });
    
        decode
    }
    
    /// Convert to Long Algebraic Notation.
    /// ### Panics:
    /// Panics if `board` and `piece` are contradictory.
    pub fn to_lan(self, board: &Board) -> String {
        if self == Move::KS_CASTLE { return "O-O".to_owned(); }
        if self == Move::QS_CASTLE { return "O-O-O".to_owned(); }

        let mut san = String::with_capacity(5);

        assert!(board.actv & self.from.bm() != 0);
        let board_piece = board.get_piece_at(self.from).unwrap();

        // piece id
        match board_piece.to_char() {
            'P' => (), // pawn id is usually suppressed
            ch  => san.push(ch),
        }

        // from square (not supressed in LAN)
        san.push_str(self.from.to_alg().as_str());

        // capture
        if board.idle & self.to.bm() != 0 { san.push('x'); }

        // to square
        san.push_str(self.to.to_alg().as_str());

        // promotion
        if board_piece != self.piece {
            assert!(board_piece == Piece::Pawn && 0xFF000000000000 & self.from.bm() != 0);
            
            san.push('=');
            san.push(self.piece.to_char());
        }

        // check & mate
        let mut b = board.clone();
        assert!(b.is_valid(self));
        b.make(self);
        if b.is_actv_in_check() {
            san.push(if let Some(GameOver::Checkmate) = b.is_mate() { '#' } else { '+' });
        }

        san
    }
}


impl Default for Board {
    fn default() -> Self {
        Self::from_fen(Self::START_POS_FEN).unwrap()
    }
}

impl Board {
    pub const START_POS_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    
    /// Deserialize position from FEN string. Move counter and halfmove 
    /// clock values are not necessary, i.e. EPD position data is parseable.
    /// ### Returns:
    /// * `Ok(Board)` on successful parse.
    /// * `Err(())` upon either parse error or illegal position (`idle` in check).
    pub fn from_fen(fen: &str) -> Result<Self, &'static str> {
        // decompose string
        let mut split = fen.split_whitespace();
        let pieces = split.next().ok_or("not enough substrings")?;
        let colour = split.next().ok_or("not enough substrings")?;
        let castle_cap = split.next().ok_or("not enough substrings")?;
        let en_passant = split.next().ok_or("not enough substrings")?;

        let fifty_move_clock = if let Some(fmc) = split.next() {
            fmc.parse::<u16>().map_err(|_| "Fifty move clock parse failed")?
        } else { 0 };

        let move_count = if let Some(mc) = split.next() {
            mc.parse::<u16>().map_err(|_| "Move counter parse failed")?
        } else { 1 };


        // determine en passant status
        let mut en_passant = if en_passant == "-" {
            0
        } else if en_passant.len() == 2 {
            Sq::from_alg(en_passant).ok_or("En Passant notation parse failed")?.bm()
        } else {
            return Err("En Passant FEN section contains unexpected value");
        };
        
        // castling rights
        let white_castle_rights = CastleRights::new(castle_cap.contains('K'), castle_cap.contains('Q'));
        let black_castle_rights = CastleRights::new(castle_cap.contains('k'), castle_cap.contains('q'));

        // parse piece location data
        let mut white = 0u64;
        let mut black = 0u64;
        let mut queens = 0u64;
        let mut rooks = 0u64;
        let mut bishops = 0u64;
        let mut knights = 0u64;
        let mut pawns = 0u64;
        let mut white_king = None;
        let mut black_king = None;

        let mut sq = 56usize; // fen ranks are in reverse order
        for c in pieces.chars() {
            if c == '/' {
                assert!(sq % 8 == 0);
                sq -= 16;
            } else if c.is_ascii_digit() {
                sq += c.to_digit(16).unwrap() as usize;
            } else {
                if c.is_ascii_uppercase() {
                    white |= 1 << sq;
                } else {
                    black |= 1 << sq;
                }
                
                match c.to_ascii_lowercase() {
                    'p' => pawns |= 1 << sq,
                    'n' => knights |= 1 << sq,
                    'b' => bishops |= 1 << sq,
                    'r' => rooks |= 1 << sq,
                    'q' => queens |= 1 << sq,
                    'k' => if c.is_ascii_uppercase() {
                        white_king = Some(Sq::new(sq as u8));
                    } else {
                        black_king = Some(Sq::new(sq as u8));
                    },
                    _ => return Err("Invalid piece letter"),
                }
                sq += 1;
            }
        }

        // parse side-to-move and modify previous data accordingly
        let side = match colour {
            "w" => Side::White,
            "b" => Side::Black,
            _ => return Err("Invalid colour value"),
        };

        let white_king = white_king.ok_or("No white king")?.cflip(side);
        let black_king = black_king.ok_or("No black king")?.cflip(side);

        if side.is_black() {
            white = white.swap_bytes();
            black = black.swap_bytes();
            queens = queens.swap_bytes();
            rooks = rooks.swap_bytes();
            bishops = bishops.swap_bytes();
            knights = knights.swap_bytes();
            pawns = pawns.swap_bytes();
            en_passant = en_passant.swap_bytes();
        }
        
        let mut board = Self {
            hash: 0,
            all: white | black,
            actv: if side.is_white() { white } else { black },
            idle: if side.is_black() { white } else { black },
            pawns,
            bishops,
            knights,
            rooks,
            queens,
            actv_king: if side.is_white() { white_king } else { black_king },
            idle_king: if side.is_black() { white_king } else { black_king },
            en_passant,
            move_count,
            side,
            fifty_move_clock,
            actv_castle_rights: if side.is_white() { white_castle_rights } else { black_castle_rights },
            idle_castle_rights: if side.is_black() { white_castle_rights } else { black_castle_rights },
        };

        // once everything is said an done, calculate the hash
        board.hash = board.calc_hash();
        // then validate the position
        board.validate().map(|_| board)
    }

    /// Serialize position to FEN string.
    pub fn to_fen(&self, include_counters: bool) -> String {
        let mut fen = String::new();

        // make actv white and idle black
        let mut wb_board = self.clone();
        if wb_board.side == Side::Black {
            wb_board.flip();
            wb_board.en_passant = wb_board.en_passant.swap_bytes();
        }

        for rank in (0..8).rev() { // fen ranks are in reverse order
            let mut rank_str = String::new();
            let mut blank_count = 0;
            for sq in (rank * 8)..(rank * 8 + 8) {
                let bm = 1u64 << sq;

                if wb_board.all & bm == 0 {
                    blank_count += 1;
                    continue;
                } else {
                    if blank_count > 0 {
                        rank_str.push_str(blank_count.to_string().as_str());
                        blank_count = 0;
                    }
                }

                let c = wb_board.get_piece_at(Sq::new(sq)).unwrap().to_char();
                if wb_board.actv & bm != 0 {
                    rank_str.push(c)
                } else {
                    rank_str.push(c.to_ascii_lowercase())
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

        fen.push(if self.side == Side::White { 'w' } else { 'b' });
        fen.push(' ');

        if !wb_board.actv_castle_rights.any() && !wb_board.idle_castle_rights.any() {
            fen.push('-');
        } else {
            if wb_board.actv_castle_rights.kingside()  { fen.push('K'); }
            if wb_board.actv_castle_rights.queenside() { fen.push('Q'); }
            if wb_board.idle_castle_rights.kingside()  { fen.push('k'); }
            if wb_board.idle_castle_rights.queenside() { fen.push('q'); }
        }
        fen.push(' ');

        if wb_board.en_passant == 0 {
            fen.push('-');
        } else {
            let pos = Sq::lsb(wb_board.en_passant).to_alg();
            fen.push_str(pos.as_str());
        }
        
        if include_counters {
            fen.push(' ');
            fen.push_str(self.fifty_move_clock.to_string().as_str());
            fen.push(' ');
            fen.push_str(self.move_count.to_string().as_str());
        }

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

        assert_eq!(Board::START_POS_FEN, crate::board::Board::from_fen(Board::START_POS_FEN).unwrap().to_fen(true));
        assert_eq!(fen1, crate::board::Board::from_fen(fen1).unwrap().to_fen(true));
        assert_eq!(fen2, crate::board::Board::from_fen(fen2).unwrap().to_fen(true));
    }
}

