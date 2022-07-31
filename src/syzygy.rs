use std::time::Duration;

use crossbeam_channel::Sender;
use serde::Deserialize;

use crate::{Board, Move, Side, SearchInfo};


#[derive(Debug, Clone, Deserialize)]
pub struct SyzygyData {
    /// Distance to zeroing the fifty move clock.
    pub dtz: Option<i32>,
    pub precise_dtz: Option<i32>,
    /// Depth to mate.
    pub dtm: Option<i32>,
    pub checkmate: bool,
    pub stalemate: bool,
    pub variant_win: bool,
    pub variant_loss: bool,
    pub insufficient_material: bool,
    /// win, unknown, maybe-win, cursed-win, draw, blessed-loss, maybe-loss, loss
    pub category: String,

    /// Moves, best first.
    pub moves: Vec<SyzygyMove>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SyzygyMove {
    pub uci: String,
    pub san: String,

    /// Distance to zeroing the fifty move clock.
    pub dtz: Option<i32>,
    pub precise_dtz: Option<i32>,
    /// Depth to mate.
    pub dtm: Option<i32>,
    pub checkmate: bool,
    pub stalemate: bool,
    pub variant_win: bool,
    pub variant_loss: bool,
    pub insufficient_material: bool,
    /// win, unknown, maybe-win, cursed-win, draw, blessed-loss, maybe-loss, loss
    pub category: String,
}


pub fn query_table_data(board: &Board, timeout: Duration) -> Option<SyzygyData> {
    if board.all.count_ones() > 7 { return None; }
    let fen = board.to_fen();

    let url = format!("http://tablebase.lichess.ovh/standard?fen={}", fen);
    let response = ureq::get(url.as_str()).timeout(timeout).call().ok()?;
    let content = response.into_reader();
    let syzygy = serde_json::from_reader::<_, SyzygyData>(content).ok()?;
    
    Some(syzygy)
}

/// Returns the best move ala Syzygy Tablebase, if available.
pub fn query_table_best_uci(board: Board, info_sndr: Sender<(SearchInfo, bool)>) {
    use crate::search::{SearchEval, eval};

    if let Some(syzygy) = query_table_data(&board, Duration::from_secs(10)) {
        if let Some(mov) = syzygy.moves.first().and_then(|m| from_uci_move_str(&board, m.uci.clone())) {
            let eval = match syzygy.dtm {
                Some(dtm) if dtm.abs() <= 128 => SearchEval::Mate(dtm as i8),
                _ => match syzygy.category.as_str() {
                    "win" => SearchEval::Normal(-eval::MATE),
                    "maybe-win" => SearchEval::Normal(-eval::MATE),
                    "cursed-win" => SearchEval::Normal(-eval::MATE),
                    "draw" => SearchEval::Normal(eval::DRAW),
                    "blessed-loss" => SearchEval::Normal(eval::MATE),
                    "maybe-loss" => SearchEval::Normal(eval::MATE),
                    "loss" => SearchEval::Normal(eval::MATE),
                    _ => SearchEval::Normal(eval::DRAW),
                },
            };
            let info = SearchInfo {
                /// only report on the best move, as it's the only that's been 'evaluated'
                evals: vec![(mov, eval)],
                depth: 0,
                sel_depth: 0,
            };
            info_sndr.send((info, true)).unwrap();
        }
    }
}



fn from_uci_move_str(board: &Board, mov: String) -> Option<Move> {
    use crate::Piece;

    if !mov.is_ascii() { return None; };
    let mov = mov.trim().as_bytes();
    if mov.len() != 4 && mov.len() != 5 { return None; }

    let mut from_sq = (mov[0] - b'a') + (mov[1] - b'1') * 8;
    let mut to_sq   = (mov[2] - b'a') + (mov[3] - b'1') * 8;
    if board.colour == Side::Black {
        from_sq = crate::board::flip_sq(from_sq);
        to_sq = crate::board::flip_sq(to_sq);
    }

    let piece = match mov.get(4).map(|&b| b.to_ascii_lowercase()) {
        Some(b'q') => Piece::Queen,
        Some(b'r') => Piece::Rook,
        Some(b'b') => Piece::Bishop,
        Some(b'n') => Piece::Knight,
        None => board.get_piece_at(1 << from_sq)?,
        Some(_) => return None,
    };
    
    let mov = Move::new(from_sq, to_sq, piece);

    board.is_valid(mov).then_some(mov)
}

#[cfg(test)]
mod tests {
    /* #[test]
    pub fn test_query_syzygy() {
        let b = crate::Board::from_fen("8/5kb1/8/8/6K1/3B4/4PP2/8 w - - 0 1").unwrap();
        eprintln!("{:#?}", super::query_table_data(&b, std::time::Duration::from_secs(3)));
        panic!();
    } */
}

