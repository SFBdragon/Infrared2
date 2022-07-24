pub mod movegen;
pub mod eval;
pub mod time;
pub mod ttab;


use std::sync::{Arc, atomic::{AtomicBool, Ordering::SeqCst}};
use crossbeam_channel::Sender;
use rayon::prelude::*;

use crate::{Board, Move, PosHashMap, board::zobrist::PosHashNode};
use ttab::{TransTable, TtData, SearchNode, ScoreKind};
use eval::eval;


/// Reported evaluation information.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchEval {
    /// Returns the position score as the side-to-move in centipawns.
    Normal(i16),
    /// Returns the distance until mate in plies.
    Mate(i8),
}

/// Reported search information.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SearchInfo {
    /// Engine evaluation for side-to-move.
    pub eval: SearchEval,
    /// Best move prediction.
    pub best: Move,
    
    pub depth: u8,
    pub sel_depth: u8,
}

pub fn search(
    board: Board,
    /* time_control: TimeControl,  */
    info_sndr: Sender<(SearchInfo, bool)>, 
    trans_table: Arc<TransTable>,
    pos_hash_map: Arc<PosHashMap>,
    kill_switch: Arc<AtomicBool>,
) {
    let mut moves = Vec::new();
    board.for_mov(|mov| { moves.push((mov, 0)); false });

    let thread_pool = rayon::ThreadPoolBuilder::new().build().unwrap();

    // Full PVS for root node search
    for draft in 2.. {
        if kill_switch.load(SeqCst) { return; }
        thread_pool.install(|| {
            /* let max = AtomicI16::new(-i16::MAX); */
            
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
                        check_exts: 0,
                        killers: vec![(None, None); 24],
                        /* counters: [[None; 64]; 6], */
                        /* history: [[0u16; 64]; 6],
                        move_buff: Vec::with_capacity(1000), */
                    };
        
                    let mut board = board.clone();
                    board.make(*root_move);
        
                    /* let alpha = max.load(SeqCst); */
                    //if score > alpha {
                        /* let guess = *root_score;
                        let mut lo = guess - 100;
                        let mut hi = guess + 100;
                        let score = loop {
                            let score = -negamax(&mut board, -hi, -lo/* -alpha as i16 */, draft, &mut search_data, None);
                            
                            if score <= lo { lo = (lo - guess) * 2 + guess; /* eprintln!("l"); */ }
                            else if score >= hi { hi = (hi - guess) * 2 + guess; /* eprintln!("h"); */ }
                            else { /* eprintln!("s"); */ break score; }
                        }; */
                    
                    //let /* mut */ score/*  = -i16::MAX */;
                    //if alpha != -i16::MAX {
                    //    score = -negamax(&mut board, -alpha-1, -alpha, draft, 0, &mut search_data, None);
                    //}
                    //if alpha == -i16::MAX || score > alpha {
                    *root_score = -pvs(&mut board, -i16::MAX, i16::MAX, draft, 0, &mut search_data, None);
                    //}
                    //println!("score: {} | move: {:?}", score, root_move);
                    //}
        
                    //*root_score = score;
                    /* max.fetch_max(score, SeqCst); */
                }
            });
        });
        if kill_switch.load(SeqCst) { return; }

        // sort
        moves.sort_unstable_by_key(|m| -m.1);
        //eprintln!("{:?}", moves);

        // send search info
        let eval = match moves[0].1 {
            score if score <=  eval::MATE+128 => SearchEval::Mate((-score-eval::MATE-1) as i8),
            score if score >= -eval::MATE-128 => SearchEval::Mate((-score+eval::MATE+1) as i8),
            score => SearchEval::Normal(score),
        };
        let info = SearchInfo { eval, best: moves[0].0, depth: draft as u8 + 1, sel_depth: draft as u8 + 5 };
        if let Err(_) = info_sndr.send((info, false)) { panic!("No search info receiver!"); }
    }
}


/* pub fn nm(board: &Board, draft: u8, mut alpha: i16, beta: i16) -> i16 {
    if draft == 0 { return eval(board); }

    let mut move_table = crate::board::mov::MoveSetTable::new();
    board.get_move_tab_actv(&mut move_table);
    
    let mut max = -i16::MAX;
    for_mov!(mov in ai move_table until 'brk => {
        if board.is_move_legal(mov) {
            let mut board = board.clone();
            board.make(mov);
            let score = -nm(&board, draft - 1, -beta, -alpha);
            if score > max { max = score; }
            if max > alpha { alpha = max; }
            if alpha >= beta { break 'brk; }
        }
    });
    return max
} */

pub struct SearchData<'a> {
    pub kill_switch: &'a AtomicBool,
    pub trans_table: &'a TransTable,
    pub pos_hash_map: &'a PosHashMap,

    pub node_count: usize,
    pub check_exts: u8,
    pub killers: Vec<(Option<Move>, Option<Move>)>,
    /* pub counters: [[Option<Move>; 64]; 6], */
    /* pub history: [[u16; 64]; 6],
    pub move_buff: Vec<Move>, */
}

pub fn pvs(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8, data: &mut SearchData, prev_phn: Option<&PosHashNode>) -> i16 {
    if draft == 0 { return quesce(board, alpha, beta, 0, depth) ; }
    if draft >= 4 && data.kill_switch.load(SeqCst) { return -i16::MAX; }

    let og_alpha = alpha;
    
    // transposition table
    let mut pv = Option::<Move>::None;
    if let Some(search_node) = data.trans_table.get(board.hash) {
        match search_node {
            SearchNode::Normal { score_kind: kind, data: TtData { score: sn_score, draft: sn_draft, pv: sn_pv } } => {
                let score = sn_score + depth as i16;
                if sn_draft >= draft {
                    // search is deeper, hence is sufficient to be decisive
                    match kind {
                        ScoreKind::Exact => return score,
                        ScoreKind::LoBound => if score >= beta  { return score; },
                        ScoreKind::HiBound => if score <= alpha { return score; },
                    }
                }

                pv = Some(sn_pv);
            },
            SearchNode::Checkmate => return eval::MATE + depth as i16,
            SearchNode::Stalemate => return eval::DRAW,
        }
    }

    let mut depth_reduct = 1;
    let is_actv_in_check = board.is_actv_in_check();

    /* let mut check_ext = false;
    if is_actv_in_check && depth > 3 && data.check_exts < 3 {
        draft += 1;
        check_ext = true;
        data.check_exts += 1;
    } */
    
    // null move reduction
    if draft > 3 {
        let material_eval = eval::basic_mat_eval(board);
        if !is_actv_in_check && material_eval >= 100 {
            let mut b = board.clone();
            b.make_null();
            let phn = PosHashNode::new(b.hash, prev_phn);
            let null_score = pvs(&b, -beta, -alpha, draft - 2, depth + 1, data, Some(&phn));
            if null_score >= beta { depth_reduct += 2; }
        }
    }


    let mut max = -i16::MAX;
    let mut best_move = None;
    let mut move_count = 0;
    let mut legal_move_exists = false;
    let thing = |mov: Move, board: &Board, data: &mut SearchData| -> bool {
        legal_move_exists = true;
        move_count += 1;
        if draft > 3 && move_count == 6 /* && !check_ext */ && !is_actv_in_check { depth_reduct += 1; }

        let mut b = board.clone();
        b.make(mov);
        /* board.validate().unwrap(); */
        
        let mut score = -i16::MAX;
        if is_pos_draw(&b, data.pos_hash_map, prev_phn) {
            score = eval::DRAW;
        } else {
            // else continue search
            let phn = PosHashNode::new(b.hash, prev_phn);
            if max != -i16::MAX {
                score = -pvs(&mut b, -i16::MAX/* -alpha-1 */, -alpha, draft - depth_reduct, depth + 1, data, Some(&phn));
            }
            if max == -i16::MAX || score >= alpha && score < beta {
                score = -pvs(&mut b, -beta, -alpha, draft - 1, depth + 1, data, Some(&phn));
            }
        }
        
        if score > max {
            max = score;
            best_move = Some(mov);
            if max > alpha { alpha = max; }
            if max >= beta {
                // update history heuristic
                /* let weight = draft as u16 * draft as u16;
                data.history[mov.piece as usize][mov.to_sq as usize] += weight; */
                // update countermove heuristic
                /* data.counters[prev_move.piece as usize][prev_move.to_sq as usize] = Some(mov); */
                return true;
            }
        }

        false
    };

    
    if draft > 2 {
        movegen::gen(thing, board, depth, data, pv);
    } else {
        movegen::gen_light(thing, board, depth, data, pv);
    }

    /* // only-move extention
    if moves.len() == 1 { draft += 1; } */

    /* if check_ext { data.check_exts -= 1; } */

    // update ttab
    if !legal_move_exists { // no legal moves - mate
        if !board.is_actv_in_check() {
            max = eval::DRAW;
            data.trans_table.insert(board.hash, SearchNode::Stalemate, |_| true);
        } else {
            max = eval::MATE + depth as i16;
            data.trans_table.insert(board.hash, SearchNode::Checkmate, |_| true);
        }
    } else { // normal search
        let best_move = best_move.unwrap();
        // update killer move
        let (k1, k2) = data.killers.get_mut(depth as usize).unwrap();
        match k1 {
            Some(k1v) if *k1v == best_move => (),
            _ => { *k2 = *k1; *k1 = Some(best_move); },
        }
        /* // update counter move
        data.counters[prev_move.piece as usize][prev_move.to_sq as usize] = Some(best_move); */
        
        let kind;
        if max <= og_alpha { kind = ttab::ScoreKind::LoBound; }
        else if max < beta { kind = ttab::ScoreKind::Exact; }
        else { kind = ttab::ScoreKind::HiBound; }

        let tt_node = SearchNode::Normal {
            score_kind: kind,
            data: TtData { score: max - depth as i16, draft, pv: best_move }
        };
        data.trans_table.insert(board.hash, tt_node, |old| {
            if let SearchNode::Normal { score_kind: _, data: TtData { draft: old_draft, score: _, pv: _ } } = old {
                *old_draft <= draft // update if draft is at least as good
            } else {
                true // always-replace policy for mates
            }
            //true
        });
    }
    
    max
}

fn quesce(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8) -> i16 {
    if draft <= -4 { return eval(board); }

    let is_actv_in_check = board.is_actv_in_check();
    let mut max = -i16::MAX;

    if !is_actv_in_check { // no stand pat in check
        max = eval(board); // stand pat
        if max >= beta { return max; }
        if max > alpha { alpha = max; }
    }

    let mut quiet_position = true;
    let thing = |mov: Move, board: &Board| -> bool {
        quiet_position = false;

        let mut board = board.clone();
        board.make(mov);
        let score = -quesce(&board, -beta, -alpha, draft - 1, depth + 1);

        if score > max {
            max = score;
            if score >= beta { return true; }
            if score > alpha { alpha = score; }
        }
        false
    };
    movegen::gen_quesce(thing, board, is_actv_in_check);

    if quiet_position {
        if is_actv_in_check {
            eval::MATE + depth as i16
        } else {
            eval(board)
        }
    } else {
        max
    }
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

