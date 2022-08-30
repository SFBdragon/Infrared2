pub mod ord;
pub mod eval;
pub mod time;
pub mod htab;


use std::{
    sync::{Arc, atomic::{AtomicBool, Ordering::SeqCst, AtomicI8}, Mutex}, 
    time::{Duration, Instant}, 
    collections::HashMap,
    thread, 
};
use crossbeam_channel::Sender;

use crate::{Board, Move, PosHashMap, board::zobrist::PosHashNode};
use htab::{SearchNode, ScoreKind, TransTable, PkEvalTable};
use time::{AllocatedTime, TimeManager};

/// Assumed unreachable depth.
const MAX_DEPTH: usize = 128;
const FIRST_ITER_DEPTH: i8 = 2;

pub struct PersistantData(TransTable, PkEvalTable);

impl PersistantData {
    pub fn new() -> Self {
        Self(
            TransTable::with_memory(htab::TRANS_MEM_DEFAULT),
            PkEvalTable::with_memory(htab::PK_EVAL_MEM_DEFAULT),
        )
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
            s if s <=  eval::MATE + MAX_DEPTH as i16 => SearchEval::Mate((-s+eval::MATE-1) as i8),
            s if s >= -eval::MATE - MAX_DEPTH as i16 => SearchEval::Mate((-s-eval::MATE  ) as i8),
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
    /// Principal variation. First element is best move. Length is nonzero.
    pub pv: Vec<Move>,
    /// Position valuation from side-to-move.
    pub eval: Option<SearchEval>,
    /// Search depth: `(full, selective)`.
    pub depth: Option<(usize, usize)>,
}

/// ### Panics:
/// Panics if there are no playable moves.
pub fn search(
    board: Board,
    pos_map: PosHashMap,
    prev_move: Option<Move>,

    info_sndr: Sender<(SearchInfo, bool)>, 
    kill_switch: Arc<AtomicBool>,

    allocated_time: AllocatedTime,
    trans_table: Arc<TransTable>,
    pk_table: Arc<PkEvalTable>,
) {

    // setup

    let mut root_moves = Vec::new();
    board.for_move(|mv| { root_moves.push(mv); false });
    assert_ne!(root_moves.len(), 0);
    
    // wrapping option indicates send-ability of search data (on send, set as None)
    let best_arc = Arc::new(Mutex::new(Some((root_moves[0], None))));
    let draft_arc = Arc::new(AtomicI8::new(FIRST_ITER_DEPTH - 1));

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
    
    if let Some(tgt_time_arc) = time_tgt.clone() {
        watchdog(
            info_sndr.clone(), 
            kill_switch.clone(), 
            tgt_time_arc.clone(), 
            best_arc.clone(), 
            draft_arc.clone(),
        );
    }

    // search!

    let mut evals = HashMap::<Move, i16>::with_capacity(root_moves.len());
    let heuristics = Arc::new(Mutex::new(Heuristics::new()));

    loop {
        if kill_switch.load(SeqCst) { break; }

        let draft = draft_arc.fetch_add(1, SeqCst) + 1;
        let prev_hstx = heuristics.lock().unwrap().clone();
        let helper_kill_switch = Arc::new(AtomicBool::new(false));
        let mut helper_threads = Vec::new();

        // search helper threads
        for _ in 1..thread::available_parallelism().map_or(1, |x| x.get()) {
            let mut helper_moves = root_moves.clone();
            let helper_ks = helper_kill_switch.clone();
            let b = board.clone();
            let trans_table = trans_table.clone();
            let pos_map = pos_map.clone();
            let pk_table = pk_table.clone();
            let hstx = heuristics.lock().unwrap().clone();
            
            let handle = thread::spawn(move || {
                fastrand::shuffle(helper_moves.as_mut_slice());
                let mut alpha = -i16::MAX;
                let mut search_data = SearchData::new(&helper_ks, &trans_table, &pk_table, &pos_map, hstx);

                for mv in helper_moves {
                    let mut b = b.clone();
                    b.make(mv);

                    let score = -pvs(&b, -i16::MAX, i16::MAX, draft, 0, &mut search_data, None, Some(mv));
                    alpha = alpha.max(score);
                }
            });
            helper_threads.push(handle);
        }

        // main search thread
        let mut search_data = SearchData::new(&kill_switch, &trans_table, &pk_table, &pos_map, prev_hstx);

        let mut alpha = -i16::MAX;
        for &mv in root_moves.iter() {
            
            let mut b = board.clone();
            b.make(mv);
            
            let score = -pvs(&b, -i16::MAX, /* i16::MAX */-alpha, draft, 0, &mut search_data, None, Some(mv));
            if kill_switch.load(SeqCst) { break; }

            evals.insert(mv, score);
            
            if score > alpha {
                alpha = score;
                *best_arc.lock().unwrap() = Some((mv, Some(SearchEval::from_search(score))));
            }
        }

        helper_kill_switch.store(true, SeqCst);
        helper_threads.into_iter().for_each(|jh| jh.join().unwrap());
        
        root_moves.sort_by_key(|m| -evals[m]);
        *heuristics.lock().unwrap() = search_data.hstx;

        let best = *best_arc.lock().unwrap();
        if let Some((mv, eval)) = best {
            // collect search info
            let pv = vec![mv];
            let depth = Some((draft as usize, (draft - MAX_QUESCE_DRAFT) as usize));
            let info = SearchInfo { pv, eval, depth };

            // check in with time manager
            let is_final = kill_switch.load(SeqCst) || match time_man.as_mut().map(|man| man.update(&info)) {
                Some(Some(dur)) => { *time_tgt.as_ref().unwrap().lock().unwrap() = dur; false },
                Some(None) => true, // time manager allocates no more time; finish
                None => false, // no time manager; continue
            };

            if is_final { *best_arc.lock().unwrap() = None; }
            let _ = info_sndr.send((info, is_final));
            if !is_final { continue; }
        }

        return;
    }
}

fn watchdog(
    info_sndr: Sender<(SearchInfo, bool)>,
    kill_switch: Arc<AtomicBool>,
    tgt_time_arc: Arc<Mutex<Duration>>,
    best_arc: Arc<Mutex<Option<(Move, Option<SearchEval>)>>>, 
    draft_arc: Arc<AtomicI8>,
) {
    let search_begin = Instant::now();
    std::thread::spawn(move || {
        loop {
            let search_time = Instant::now() - search_begin;
            let tgt_time = *tgt_time_arc.lock().unwrap();

            if tgt_time > search_time + Duration::from_millis(100) {
                std::thread::sleep(
                    tgt_time - search_time - Duration::from_millis(100)
                );
            } else {
                break;
            }
        }

        let best = std::mem::replace(&mut *best_arc.lock().unwrap(), None);
        if let Some((mov, eval)) = best {
            kill_switch.store(true, SeqCst);

            // determine pv data
            let pv = vec![mov];
            let draft = draft_arc.load(SeqCst);
            let depth = Some((draft as usize, (draft - MAX_QUESCE_DRAFT) as usize));
            // Send info, regardless of channel state.
            info_sndr.send((SearchInfo { pv, eval, depth }, true)).unwrap();
        }
    });
}

#[derive(Debug, Clone)]
struct Heuristics {
    pub counter: Vec<[[[u64; 64]; 6]; 64]>,
    pub killers: [[Option<Move>; 2]; MAX_DEPTH],
    pub history: [[u64; 64]; 6],
}
impl Heuristics {
    pub fn new() -> Self {
        Self {
            counter: vec![[[[0u64; 64]; 6]; 64]; 6],
            killers: [[None;  2]; MAX_DEPTH],
            history: [[0u64; 64]; 6],
        }
    }
}

#[derive(Clone)]
struct SearchData<'a> {
    pub kill_switch: &'a AtomicBool,
    pub trans_table: &'a TransTable,
    pub pk_eval_table: &'a PkEvalTable,
    pub pos_hash_map: &'a PosHashMap,

    /* pub node_count: usize,
    pub pv: Vec<Option<Move>>, // make shared? somehow? */ 
    pub check_exts: u8,
    pub hstx: Heuristics,
}
impl<'a> SearchData<'a> {
    pub fn new(
        kill_switch: &'a AtomicBool,
        trans_table: &'a TransTable,
        pk_eval_table: &'a PkEvalTable,
        pos_hash_map: &'a PosHashMap,
        hstx: Heuristics,
    ) -> Self {
        Self {
            kill_switch,
            trans_table,
            pk_eval_table,
            pos_hash_map,
            /* node_count: 0,
            pv: vec![None; MAX_DEPTH], */
            check_exts: 0,
            hstx,
        }
    }
}

fn pvs(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8, data: &mut SearchData, prev_phn: Option<&PosHashNode>, prev_move: Option<Move>) -> i16 {
    if draft <= 0 { return quesce(board, alpha, beta, draft, depth, data, prev_move.unwrap()); }
    if draft >= 3 && data.kill_switch.load(SeqCst) { return -i16::MAX; }

    let og_alpha = alpha;
    
    // transposition table
    let mut pv = Option::<Move>::None;
    if let Some(search_node) = data.trans_table.get(board.hash) {
        match search_node {
            SearchNode::Score { score_kind: kind, score, draft: sn_draft, pv: sn_pv } => {
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


    /* let mut mvbf = MoveBuffer::new();

    if data.left_line {
        if draft > 5 || draft > 1 && pv.is_none() {
            board.for_move(|mv| { mvbf.push(mv); false });
            let moves = mvbf.as_mut_slice();
            let mut scores = fnv::FnvHashMap::with_capacity_and_hasher(moves.len(), Default::default());
            
            for d in 1..draft {
                let mut a = alpha;
                for m in 0..moves.len() {
                    let mv = moves[m];
                    let mut b = board.clone();
                    b.make(mv);
                    let phn = PosHashNode::new(b.hash, prev_phn);
                    let score = -pvs(&b, -beta, -a, d, depth + 1, data, Some(&phn), mv);
                    scores.insert(mv, score);
                    a = a.max(score);
                }

                moves.sort_by_key(|mv| scores[mv]);
            }

        } else if draft == 1 {
            data.left_line = false;
        }
    } */


    let in_check = board.in_check();

    let mut check_ext = false;
    if in_check && draft <= 2 && data.check_exts < 1 {
        check_ext = true;
        data.check_exts += 1;
    }
    
    // null move pruning
    if draft > 3 && !in_check && board.actv & !board.pawns & !board.actv_king.bm() != 0 {
        let material_eval = eval::pesto(board);//eval::eval(board, data.pk_eval_table);
        if material_eval >= beta {
            let mut b = board.clone();
            b.make_null();
            //if let Err(e) = b.validate() { panic!("{}\n{}\n{}\n\n", e, board.to_fen(true), b.to_fen(true)); }

            let phn = PosHashNode::new(b.hash, prev_phn);
            let null_score = -pvs(&b, -beta, -alpha, draft - 3, depth + 1, data, Some(&phn), None);

            if null_score >= beta { return beta; }
        }
    }


    let mut best: Option<(i16, Move)> = None;
    let mut move_count = 0;
    let thing = |mv: Move, board: &Board, data: &mut SearchData| -> bool {
        move_count += 1;
        let mut b = board.clone();
        b.make(mv);
        //if let Err(e) = b.validate() { panic!("{}\n{:?}\n{}\n{}\n\n", e, mv, board.to_fen(true), b.to_fen(true)); }

        let mut score;
        if is_pos_draw(&b, data.pos_hash_map, prev_phn) {
            score = eval::DRAW;
        } else {
            let phn = PosHashNode::new(b.hash, prev_phn);

            let child_draft = draft - 1 + check_ext as i8 * 2;

            if best.is_none() || pv.is_none() && alpha == og_alpha {
                score = -pvs(&b, -beta, -alpha, child_draft, depth + 1, data, Some(&phn), Some(mv));
            } else {
                score = -pvs(&b, -alpha-1, -alpha, child_draft, depth + 1, data, Some(&phn), Some(mv));
                if alpha <= score && score < beta {
                    // test: reset depth reduct?
                    score = -pvs(&b, -beta, -alpha, child_draft, depth + 1, data, Some(&phn), Some(mv));
                }
            }
        }
        
        if best.map_or(true, |b| b.0 <= score) {
            best = Some((score, mv));
            if score > alpha { alpha = score; }
            if score >= beta {

                return true;
            }
        }

        false
    };
    
    let full = beta.saturating_sub(og_alpha) > 1 && draft > 2;
    ord::ord(thing, board, depth, data, pv, prev_move, full);

    if check_ext { data.check_exts -= 1; }

    // update ttab
    if let Some((score, mv)) = best {
        if board.all & mv.to.bm() == 0 {
            // update killer move
            debug_assert!((depth as usize) < data.hstx.killers.len());
            let [k1, k2] = unsafe { data.hstx.killers.get_unchecked_mut(depth as usize) };
            match k1 {
                Some(k1v) if *k1v == mv => (),
                _ => { *k2 = *k1; *k1 = Some(mv); },
            }

            let weight = (draft as u64).pow(2);
            // update history heuristic
            data.hstx.history[mv.piece as usize][mv.to.us()] += weight;
            if let Some(prev) = prev_move {
                // update countermove heuristic
                data.hstx.counter[prev.piece as usize][prev.to.us()][mv.piece as usize][mv.to.us()] += weight;
            }
        }
        
        let score_kind = match score {
            score if score <= og_alpha => ScoreKind::HiBound,
            score if score < beta => ScoreKind::Exact,
            _ => ScoreKind::LoBound,
        };
        let tt_node = SearchNode::Score { score_kind, score, draft, pv: mv };
        data.trans_table.insert(board.hash, tt_node, |&old| {
            if let SearchNode::Score { draft: old_draft, score_kind: old_kind, .. } = old {
                // update if draft is at least as good, or score_kind is better and draft is equal
                old_draft < draft || (old_kind != ScoreKind::Exact && old_draft == draft) 
            } else { true } // always-replace policy for mates
        });

        score
    } else { // no legal moves - mate
        if !in_check {
            data.trans_table.insert(board.hash, SearchNode::Stalemate, |_| true);
            eval::DRAW
        } else {
            data.trans_table.insert(board.hash, SearchNode::Checkmate, |_| true);
            eval::MATE + depth as i16
        }
    }
}

const MAX_QUESCE_DRAFT: i8 = -4;

fn quesce(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8, data: &mut SearchData, prev_move: Move) -> i16 {
    if draft <= MAX_QUESCE_DRAFT { return eval::pesto(board)/* eval(board, data.pk_eval_table); */ }

    let in_check = board.in_check();
    let mut max = -i16::MAX;

    if !in_check { // no stand pat in check
        max = eval::pesto(board);//eval(board, data.pk_eval_table); // stand pat
        if max >= beta { return max; }
        if max > alpha { alpha = max; }
    }

    let mut quiet_position = true;
    let thing = |mov: Move, board: &Board| -> bool {
        quiet_position = false;

        let mut board = board.clone();
        board.make(mov);
        let score = -quesce(&board, -beta, -alpha, draft - 1, depth + 1, data, mov);

        if score > max {
            max = score;
            if score >= beta { return true; }
            if score > alpha { alpha = score; }
        }
        false
    };
    ord::ord_quesce(thing, board, in_check, prev_move);

    if quiet_position {
        if in_check {
            eval::UNCERTAIN_MATE + depth as i16
        } else {
            eval::pesto(board)//eval(board, data.pk_eval_table)
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
