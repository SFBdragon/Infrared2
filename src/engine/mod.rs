use std::{thread, sync::{Arc, atomic::{AtomicBool, Ordering::SeqCst, AtomicI16}}};

use crossbeam_channel::Sender;
use rayon::prelude::*;

use crate::{Board, Move, for_mov, PosHashMap, board::zobrist::PosHashNode, engine::eval::eval};

use self::{time::TimeControl, ttab::TransTable};


pub mod eval;
pub mod open;
pub mod time;
pub mod ttab;


pub struct Infrared {
    pub board: Board,
    pub kill_switch: Arc<AtomicBool>,

}

/// Reported evaluation information.
pub enum SearchEval {
    /// Returns the position score as the side-to-move in centipawns.
    Normal(i16),
    /// Returns the distance until mate in plies.
    Mate(i8),
}

/// Reported search information.
pub struct SearchInfo {
    /// Engine evaluation for side-to-move.
    pub eval: SearchEval,
    /// Best move prediction.
    pub best: Move,
    /// Whether the engine is done searching or not.
    pub done: bool,
}

pub fn search(
    board: Board,
    /* time_control: TimeControl,  */
    info_sndr: Sender<SearchInfo>, 
    trans_table: Arc<TransTable>,
    pos_hash_map: Arc<PosHashMap>,
    kill_switch: Arc<AtomicBool>,
) {
    let mut move_table = crate::board::mov::MoveSetTable::new();
    board.get_move_tab_actv(&mut move_table);
    let mut moves = Vec::new();
    for_mov!(mov in ai move_table => {
        if board.is_move_legal(mov) {
            moves.push((mov, 0));
        }
    });

    let thread_count = thread::available_parallelism().map_or(1, |nzu| nzu.get());
    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(thread_count)
        .build()
        .unwrap();

    // Full PVS for root node search
    for draft in 2.. {
        if kill_switch.load(SeqCst) { return; }
        thread_pool.install(|| {
            let max = AtomicI16::new(-i16::MAX);
            
            moves.par_iter_mut().for_each(|(root_move, root_score)| {
                if !kill_switch.load(SeqCst) {
                    let trans_table = trans_table.clone();
                    let pos_hash_map = pos_hash_map.clone();
                    let kill_switch = kill_switch.clone();

                    let mut search_data = SearchData {
                        kill_switch: &kill_switch,
                        trans_table: &trans_table,
                        pos_hash_map: &pos_hash_map,

                        node_count: 0,
                      
                        //null_prune: false,
                        check_exts: 0,
                    };

                    let alpha = max.load(SeqCst);
        
                    let mut board = board.clone();
                    board.make(*root_move);
        
                    //let mut score = -negamax(&board, -alpha-1, -alpha, 0, draft, kill_switch.as_ref());
                    //if score > alpha {
                    let score = -negamax(&mut board, -i16::MAX, /* f32::INFINITY */-alpha as i16, draft, &mut search_data, None);
                    //println!("score: {} | move: {:?}", score, root_move);
                    //}
        
                    *root_score = score;
                    max.fetch_max(score, SeqCst);
                }
            });
        });
        if kill_switch.load(SeqCst) { return; }

        // sort
        moves.sort_unstable_by_key(|m| -m.1);
        //eprintln!("{:?}", moves);

        // send search info
        let eval = match moves[0].1 {
            score if score <= eval::MATE => SearchEval::Mate((score - eval::MATE) as i8),
            score if score >= -eval::MATE => SearchEval::Mate((score + eval::MATE) as i8),
            score => SearchEval::Normal(score),
        };
        let info = SearchInfo { eval, best: moves[0].0, done: false };
        if let Err(_) = info_sndr.send(info) { panic!("No search info receiver!"); }
    }
}

#[derive(Debug)]
pub struct SearchData<'a> {
    pub kill_switch: &'a AtomicBool,
    pub trans_table: &'a TransTable,
    pub pos_hash_map: &'a PosHashMap,

    pub node_count: usize,

    //pub null_prune: bool,
    pub check_exts: u8,
    //pub killers: Vec<(Move, Move)>,
}

pub fn negamax(board: &mut Board, mut alpha: i16, beta: i16, draft: i8, data: &mut SearchData, prev_phn: Option<&PosHashNode>) -> i16 {
    if draft == 0 { return eval::eval(board) /* quesce(board, alpha, beta, 0) */ ; }
    if draft >= 4 && data.kill_switch.load(SeqCst) { return -i16::MAX; }

    let og_alpha = alpha;
    let mut pv = Option::<Move>::None;
    let mut max = -i16::MAX;

    // null move pruning
    let is_actv_in_check = board.is_actv_in_check();
    let material_eval = eval::basic_mat_eval(board);
    let king_and_pawns = board.actv & board.pawns & (1 << board.actv_king_sq) == board.actv;
    if draft > 2 && !king_and_pawns && !is_actv_in_check && material_eval >= 100 {
        board.make_null();
        let phn = PosHashNode::new(board.hash, prev_phn);
        //data.null_prune = true;
        let null_score = negamax(board, -beta, -alpha, draft - 2, data, Some(&phn));
        //data.null_prune = false;
        board.unmake_null();
        if null_score >= beta { return null_score; }
    }
    
    //if draft > 1 {
    /* if let Some(search_node) = data.trans_table.get(board.hash) {
        if search_node.draft >= draft {
            // this search is at least as deep as this one, thus its score is useful
            let search_score = search_node.score;
            match search_node.kind {
                ttab::ScoreKind::Exact => return search_score,
                ttab::ScoreKind::LoBound => if search_score >= beta { return search_score; },
                ttab::ScoreKind::HiBound => if search_score <= alpha { return search_score; },
            }
        }
        if let Some(tt_pv) = search_node.pv {
            //if board.is_move_legal(tt_pv) {
            pv = Some(tt_pv);

            let mut board = board.clone();
            board.make(tt_pv);
            
            if is_pos_draw(&board, data.pos_hash_map, prev_phn) {
                max = eval::DRAW;
            } else {
                // else continue search
                let phn = PosHashNode::new(board.hash, prev_phn);
                max = -negamax(&mut board, -beta, -alpha, draft - 1, data, Some(&phn));
            }

            if max > alpha {
                alpha = max;
                if max >= beta {
                    return max;
                }
            }
            //}
        }
    } */
    //}

    let mut move_table = crate::board::mov::MoveSetTable::new();
    board.get_move_tab_actv(&mut move_table);

    let mut legal_move_exists = false;
    let mut best_move = pv;
    for_mov!(mov in ai move_table until 'brk => {

        if let Some(pv) = pv { if mov == pv { continue; } }
        if board.is_move_legal(mov) {
            legal_move_exists = true;
            
            let mut board = board.clone();
            board.make(mov);
            if board.is_idle_in_check() { dbg!(&board); dbg!(board.to_fen()); dbg!(mov); panic!(); }
            
            let mut score = -i16::MAX;

            if is_pos_draw(&board, data.pos_hash_map, prev_phn) {
                score = eval::DRAW;
            } else {
                // else continue search
                let phn = PosHashNode::new(board.hash, prev_phn);
                //if pv.is_some() {
                //    score = -negamax(&mut board, -alpha-1, -alpha, draft - 1, data, Some(&phn));
                //}
                //if pv.is_none() || score > alpha && score < beta {
                    score = -negamax(&mut board, -beta, -alpha, draft - 1, data, Some(&phn));
                //    pv = None;
                //}
            }
            
            if score > max {
                max = score;
                best_move = Some(mov);
                if max > alpha {
                    alpha = max;
                    if max >= beta {
                        break 'brk;
                    }
                }
            }
        }
    });


    let kind;
    if !legal_move_exists {
        max = if !board.is_actv_in_check() { eval::DRAW } else { eval::MATE - draft as i16 };
        kind = ttab::ScoreKind::Exact;
    } else {
        // if max is a lower bound due to FAIL HIGH
        if max <= og_alpha { kind = ttab::ScoreKind::LoBound; }
        // if max is exact
        else if max < beta { kind = ttab::ScoreKind::Exact; }
        // max is the highest upper bound (-child FAIL HIGH lower bound)
        else { kind = ttab::ScoreKind::HiBound; }
    }

    // update ttab
    let tt_node = ttab::SearchNode { score: max, kind, draft, pv: best_move };
    data.trans_table.insert(board.hash, tt_node, |new, old| old.draft <= new.draft);
    
    max
}

fn quesce(board: &Board, mut alpha: i16, beta: i16, draft: i8) -> i16 {
    let stand_pat = eval(board) + draft as i16;
    let mut max = stand_pat;
    if max >= beta || draft <= -3 { return max; }
    if max > alpha { alpha = max; }

    let mut move_table = crate::board::mov::MoveSetTable::new();
    board.get_move_tab_actv(&mut move_table);

    let targets = board.idle & !board.pawns;
    let is_actv_in_check = board.is_actv_in_check();

    let mut quiet_position = true;
    for_mov!(mov in ai move_table => {
        let to = 1u64 << mov.to_sq;
        if is_actv_in_check || ((to & targets != 0) /* && stand_pat + eval::lerp(eval::PIECE_MATERIAL[board.get_piece_at(to).unwrap() as usize], 0.5) + 200.0 >= alpha */) {
            if board.is_move_legal(mov) {
                quiet_position = false;

                let mut board = board.clone();
                board.make(mov);
                let score = -quesce(&board, -beta, -alpha, draft - 1);

                if score > max {
                    max = score;
                    if score >= beta { return score; }
                    if score > alpha { alpha = score; }
                }
            }
        }
    });

    // add draft minisculy incentivises achieving the same score at a higher depth
    if quiet_position { eval(board) + draft as i16 } else { max }
}

#[inline]
pub fn is_pos_draw(board: &Board, phm: &PosHashMap, prev_phn: Option<&PosHashNode>) -> bool {
    let mut loop_phn_ref = prev_phn.and_then(|pphn| pphn.prev);

    // check if position has occured twice prior in the game
    phm.get(&board.hash).map_or(false, |&c| c >= 2) 

    // check if position has occured once prior towards the root
    || loop {
        if let Some(hnf) = loop_phn_ref {
            if hnf.hash == board.hash { break true; }
            loop_phn_ref = hnf.prev_prev;
        } else { break false; }
    } 

    // check for fifty move rule and insufficient material
    || board.is_draw().is_some()
}

#[cfg(test)]
mod tests {
    use super::*;
     
    /* #[test]
    fn test_negamax() {
        let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
        let mut board = crate::board::Board::from_fen(fen).unwrap();
        let mut ttab = ttab::TransTable::with_memory(1024 * 1024 * 1024 * 2);
        let mut ks = std::sync::atomic::AtomicBool::new(false);
        
        let score = -negamax(&board, -i32::MAX, i32::MAX, /* 0, */ 6, &mut ttab, &ks);
        dbg!(score);
    } */
}

