pub mod movegen;
pub mod eval;
pub mod time;
pub mod ttab;


use std::{sync::{Arc, atomic::{AtomicBool, Ordering::SeqCst, AtomicI8, /* AtomicI16 */}, Mutex}, time::{Duration, Instant}};
use crossbeam_channel::Sender;
use rayon::prelude::*;

use crate::{Board, Move, PosHashMap, board::zobrist::PosHashNode, search::time::TimeManager};
use ttab::{TransTable, TtData, SearchNode, ScoreKind};
use eval::eval;
use time::AllocatedTime;



pub struct PersistantData(TransTable);

impl PersistantData {
    pub fn new() -> Self {
        Self(TransTable::default()) // todo: make this customizable
    }
}

/// Evaluation information.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchEval {
    /// Returns the position score as the side-to-move in centipawns.
    Normal(i16),
    /// Returns the distance until mate in plies.
    Mate(i8),
}
impl SearchEval {
    fn from_search(score: i16) -> Self {
        match score {
            s if s <=  eval::MATE+128 => SearchEval::Mate((-s-eval::MATE-1) as i8),
            s if s >= -eval::MATE-128 => SearchEval::Mate((-s+eval::MATE  ) as i8),
            s => SearchEval::Normal(s),
        }
    }
}
impl Ord for SearchEval {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::{Greater, Less};
        match *self {
            SearchEval::Normal(self_score) => match other {
                SearchEval::Normal(other_score) => self_score.cmp(other_score),
                SearchEval::Mate(other_mate) => if *other_mate >= 0 { Less } else { Greater },
            }
            SearchEval::Mate(self_mate) => match other {
                SearchEval::Normal(_) => if self_mate >= 0 { Greater } else { Less },
                SearchEval::Mate(other_mate) => {
                    let is_self_pos = self_mate >= 0;
                    let is_other_pos = *other_mate >= 0;
                    if is_self_pos && !is_other_pos {
                        Greater
                    } else if !is_self_pos && is_other_pos {
                        Less
                    } else {
                        other_mate.cmp(&self_mate)
                    }
                }
            },
        }
    }
}
impl PartialOrd for SearchEval {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Reported search information.
#[derive(Debug, Clone)]
pub struct SearchInfo {
    /// Engine move evaluations.
    pub evals: Vec<(Move, SearchEval)>,
    
    /// Search depth.
    pub depth: usize,
    /// Selective search depth.
    pub sel_depth: usize,
}

/// ### Panics:
/// Panics if there are no playable moves.
pub fn search(
    board: Board,
    pos_hash_map: PosHashMap,
    prev_move: Option<Move>,

    info_sndr: Sender<(SearchInfo, bool)>, 
    kill_switch: Arc<AtomicBool>,

    allocated_time: AllocatedTime,
    trans_table: Arc<TransTable>,
) {

    // setup

    let old_srch_arc = Arc::new(Mutex::new(Vec::new()));
    let mut move_count = 0;
    {
        let mut moves_guard = old_srch_arc.lock().unwrap();
        board.for_mov(|mov| {
            moves_guard.push((mov, SearchEval::Normal(0)));
            move_count += 1;
            false
        });
        assert_ne!(move_count, 0);
    }
    
    let new_srch_arc = Arc::new(Mutex::new(Vec::with_capacity(move_count)));
    let draft_arc = Arc::new(AtomicI8::new(2));

    
    // set up time management systems

    let mut time_tgt = Option::<Arc<Mutex<Duration>>>::None;
    let mut time_man = Option::<TimeManager>::None;
    
    match allocated_time {
        AllocatedTime::Forever => (),
        AllocatedTime::Fixed(time) => time_tgt = Some(Arc::new(Mutex::new(time))),
        AllocatedTime::Fancy { time, cutoff } => {
            time_tgt = Some(Arc::new(Mutex::new(time)));
            time_man = Some(TimeManager::start(time, cutoff, prev_move));
        },
    }
    
    let pv_send_lock = Arc::new(AtomicBool::new(false));
    if let Some(tgt_time_arc) = time_tgt.clone() {
        watchdog(
            info_sndr.clone(), 
            kill_switch.clone(), 
            pv_send_lock.clone(),
            tgt_time_arc.clone(), 
            old_srch_arc.clone(), 
            new_srch_arc.clone(), 
            draft_arc.clone(),
        );
    }

    // search!

    let thread_pool = rayon::ThreadPoolBuilder::new().build().unwrap();
    loop {
        let draft = draft_arc.load(SeqCst);
        //let alpha = Arc::new(AtomicI16::new(-i16::MAX + 200));

        thread_pool.install(|| {
            old_srch_arc.lock().unwrap().clone().par_iter().for_each(|&(mov, _)| {
                if kill_switch.load(SeqCst) { return; }

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
                };
    
                let mut board = board.clone();
                board.make(mov);
                
                //let a = alpha.load(SeqCst);
                let score = -pvs(&mut board, -i16::MAX, i16::MAX, draft, 0, &mut search_data, None);
                //let score = -pvs(&mut board, -i16::MAX, -a + 150, draft, 0, &mut search_data, None);
                if kill_switch.load(SeqCst) { return; }

                let eval = SearchEval::from_search(score);
                let mut searches = new_srch_arc.lock().unwrap();
                let index = searches.binary_search_by(|(_, s)| eval.cmp(&s)).unwrap_or_else(|e| e);
                searches.insert(index, (mov, eval));

                //alpha.store(score, SeqCst);
            });
        });

        // use old searches if pv is not re-searched (occurs upon early search kill)
        let old_pv = old_srch_arc.lock().unwrap().first().unwrap().0;
        let is_pv_searched = new_srch_arc.lock().unwrap().iter().any(|&(m, _)| m == old_pv);
        let evals = match kill_switch.load(SeqCst) && !is_pv_searched {
            true =>  old_srch_arc.lock().unwrap().clone(),
            false => new_srch_arc.lock().unwrap().clone(),
        };
        
        // send search info
        let depth = draft as usize + 1;
        let info = SearchInfo { evals, depth, sel_depth: depth + 4 };
        let mut is_final = false;

        if let Some(man) = time_man.as_mut() {
            match man.update(&info) {
                Some(dur) => *time_tgt.as_ref().unwrap().lock().unwrap() = dur,
                None => is_final = true,
            }
        }
        
        if is_final {
            // if pv_send_lock has been toggled, do not send another pv!
            if let Ok(_) = pv_send_lock.compare_exchange(false, true, SeqCst, SeqCst) {
                info_sndr.send((info, true)).unwrap();
            }
            return;
        }

        if let Err(_) = info_sndr.send((info, false)) { return; }
        
        draft_arc.fetch_add(1, SeqCst);
        *old_srch_arc.lock().unwrap() = std::mem::replace(
            new_srch_arc.lock().unwrap().as_mut(), 
            Vec::with_capacity(move_count),
        );

        if kill_switch.load(SeqCst) { break; }
        if pv_send_lock.load(SeqCst) { break; }
    }
}

fn watchdog(
    info_sndr: Sender<(SearchInfo, bool)>,
    kill_switch: Arc<AtomicBool>,
    pv_send_lock: Arc<AtomicBool>,
    tgt_time_arc: Arc<Mutex<Duration>>,
    old_srch_arc: Arc<Mutex<Vec<(Move, SearchEval)>>>, 
    new_srch_arc: Arc<Mutex<Vec<(Move, SearchEval)>>>,
    draft_arc: Arc<AtomicI8>,
) {
    let search_begin = Instant::now();
    std::thread::spawn(move || {
        loop {
            let search_time = Instant::now() - search_begin;
            let tgt_time = *tgt_time_arc.lock().unwrap();

            if search_time < tgt_time {
                std::thread::sleep(
                    tgt_time - search_time - Duration::from_millis(1)
                );
            } else {
                break;
            }
        }

        // if pv_send_lock has been toggled, do not send another pv!
        if let Err(_) = pv_send_lock.compare_exchange(false, true, SeqCst, SeqCst) {
            return;
        }

        kill_switch.store(true, SeqCst);
        
        // determine if most recent depth's search has searched the pv
        let old_searches = old_srch_arc.lock().unwrap();
        let old_pv = old_searches[0].0;
        let new_searches = new_srch_arc.lock().unwrap();

        // determine pv data
        let depth = draft_arc.load(SeqCst) as usize + 1;
        let evals = match new_searches.iter().any(|&(m, _)| m == old_pv) {
            true => new_searches.clone(),
            false => old_searches.clone(),
        };

        // Send info. The channel may be disconnected, but it doesn't matter.
        let _ = info_sndr.send((SearchInfo { evals, depth, sel_depth: depth + 4 }, true));
    });
}


pub struct SearchData<'a> {
    pub kill_switch: &'a AtomicBool,
    pub trans_table: &'a TransTable,
    pub pos_hash_map: &'a PosHashMap,

    pub node_count: usize,
    pub check_exts: u8,
    pub killers: Vec<(Option<Move>, Option<Move>)>,
}

fn pvs(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8, data: &mut SearchData, prev_phn: Option<&PosHashNode>) -> i16 {
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
        //if !board.is_valid(mov) { panic!("{:?}\n{}\n\n", mov, board.to_fen()); }

        legal_move_exists = true;
        move_count += 1;
        if draft > 3 && move_count == 6 /* && !check_ext */ && !is_actv_in_check { depth_reduct += 1; }

        let mut b = board.clone();
        b.make(mov);
        //if let Err(e) = b.validate() { panic!("{}\n{:?}\n{}\n{}\n\n", e, mov, board.to_fen(), b.to_fen()); }
        
        let mut score = -i16::MAX;
        if is_pos_draw(&b, data.pos_hash_map, prev_phn) {
            score = eval::DRAW;
        } else {
            // else continue search
            let phn = PosHashNode::new(b.hash, prev_phn);
            if max != -i16::MAX {
                score = -pvs(&mut b, -i16::MAX/* -alpha-1 */, -alpha, draft - depth_reduct, depth + 1, data, Some(&phn));
            }
            if score >= alpha && score < beta || max == -i16::MAX {
                score = -pvs(&mut b, -beta, -alpha, draft - 1, depth + 1, data, Some(&phn));
            }
        }
        
        if score >= max {
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

    #[test]
    fn test_search_eval_cmp() {
        use std::cmp::Ordering;

        let s1 = SearchEval::Normal(34);
        let s2 = SearchEval::Normal(-40);
        let s3 = SearchEval::Mate(4);
        let s4 = SearchEval::Mate(8);
        let s5 = SearchEval::Mate(-2);

        assert_eq!(s1.cmp(&s1), Ordering::Equal);
        assert_eq!(s1.cmp(&s2), Ordering::Greater);
        assert_eq!(s1.cmp(&s3), Ordering::Less);
        assert_eq!(s1.cmp(&s5), Ordering::Greater);

        assert_eq!(s2.cmp(&s1), Ordering::Less);
        assert_eq!(s2.cmp(&s2), Ordering::Equal);
        assert_eq!(s2.cmp(&s3), Ordering::Less);
        assert_eq!(s2.cmp(&s5), Ordering::Greater);
        
        assert_eq!(s3.cmp(&s2), Ordering::Greater);
        assert_eq!(s3.cmp(&s3), Ordering::Equal);
        assert_eq!(s3.cmp(&s4), Ordering::Greater);
        assert_eq!(s3.cmp(&s5), Ordering::Greater);

        assert_eq!(s5.cmp(&s2), Ordering::Less);
        assert_eq!(s5.cmp(&s3), Ordering::Less);
        assert_eq!(s5.cmp(&s4), Ordering::Less);
        assert_eq!(s5.cmp(&s5), Ordering::Equal);
    }

    #[test]
    fn test_eval_bin_search() {
        use std::cmp::Ordering;

        let evals = [
            SearchEval::Mate(4),
            SearchEval::Mate(8),
            SearchEval::Normal(34),
            SearchEval::Normal(-40),
            SearchEval::Mate(-2),
        ];

        for array in evals.windows(2) {
            assert_ne!(array[0].cmp(&array[1]), Ordering::Less);
        }
    }
}
