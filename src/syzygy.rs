use std::time::Duration;

use crossbeam_channel::Sender;
use serde::Deserialize;

use crate::{Board, Move, SearchInfo, Sq};

#[allow(unused)]
#[derive(Debug, Clone, Deserialize)]
struct RawSyzygyData {
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
    pub moves: Vec<RawSyzygyMove>,
}

#[allow(unused)]
#[derive(Debug, Clone, Deserialize)]
struct RawSyzygyMove {
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

fn query_table_data(board: &Board, timeout: Duration) -> Option<RawSyzygyData> {
    if board.all.count_ones() > 7 { return None; }
    let fen = board.to_fen(true);

    let url = format!("http://tablebase.lichess.ovh/standard?fen={}", fen);
    let response = ureq::get(url.as_str()).timeout(timeout).call().ok()?;
    let content = response.into_reader();
    let syzygy = serde_json::from_reader::<_, RawSyzygyData>(content).ok()?;
    
    Some(syzygy)
}

/// Returns the best move ala Syzygy Tablebase, if available.
pub fn uci_syzygy_query(board: &Board, info_sndr: Sender<SearchInfo>) {
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
            info_sndr.send(info).unwrap();
        }
    }
}



fn from_uci_move_str(board: &Board, mov: String) -> Option<Move> {
    use crate::Piece;
    
    let mov = mov.as_str().trim();
    if !mov.is_ascii() { return None; }
    if mov.len() != 4 && mov.len() != 5 { return None; }

    let from = Sq::from_alg(&mov[0..2])?.cflip(board.side);
    let to   = Sq::from_alg(&mov[2..4])?.cflip(board.side);

    let piece = mov.chars().skip(4).next().map_or(
        board.get_piece_at(from), 
        |ch| Piece::from_char_prom(ch.to_ascii_uppercase())
    )?;
    
    let mov = Move::new(from, to, piece);

    board.is_valid(mov).then_some(mov)
}


