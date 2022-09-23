pub mod ord;
pub mod time;
pub mod htab;


use std::{
    sync::{Arc, atomic::{AtomicBool, Ordering::SeqCst, AtomicI8}, Mutex}, 
    time::{Duration, Instant}, 
    collections::{HashMap, HashSet},
    thread, 
};
use crossbeam_channel::Sender;

use crate::{Board, Move, PosHashMap, board::{eval, zobrist::U64IdentHashBuilder}};
use htab::{SearchNode, ScoreKind, TransTable};
use time::{AllocatedTime, TimeManager};

/// Assumed unreachable depth.
const MAX_DEPTH: usize = 128;
const FIRST_ITER_DEPTH: i8 = 2;

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
    info_sndr: Sender<(SearchInfo, bool)>, 
    kill_switch: Arc<AtomicBool>,
    allocated_time: AllocatedTime,
    hash_size_mb: usize,
    thread_count: usize,
) {
    
    // ---- SETUP ---- //
    
    // A new tranposition table is used for each search due to
    // repetition and 50 move rule errors due to outdated hash-scores
    let trans_table = Arc::new(htab::TransTable::with_memory(hash_size_mb * 1024 * 1024));

    // generate available moves
    let mut root_moves = Vec::new();
    board.for_move(|mv| { root_moves.push(mv); false });
    assert_ne!(root_moves.len(), 0);
    
    // option indicates send-ability of search data (on send, set as None)
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
            time_man = Some(TimeManager::start(time, cutoff));
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

    // ---- SEARCH ---- //

    let mut double_pos_rep = HashSet::with_capacity_and_hasher(pos_map.capacity(), U64IdentHashBuilder);
    for (k, v) in pos_map { if v == 2 { double_pos_rep.insert(k); } }
    let mut evals = HashMap::<Move, i16>::with_capacity(root_moves.len());
    let heuristics = Arc::new(Mutex::new(Heuristics::new()));

    loop {
        if kill_switch.load(SeqCst) { break; }

        let draft = draft_arc.fetch_add(1, SeqCst) + 1;
        let prev_hstx = heuristics.lock().unwrap().clone();
        let helper_kill_switch = Arc::new(AtomicBool::new(false));
        let mut helper_threads = Vec::new();

        // search helper threads
        for _ in 1..thread_count.max(1) {
            let mut helper_moves = root_moves.clone();
            let board = board.clone();
            let helper_ks = helper_kill_switch.clone();
            let trans_table = trans_table.clone();
            let doubles = double_pos_rep.clone();
            let hstx = heuristics.lock().unwrap().clone();
            
            let handle = thread::spawn(move || {
                fastrand::shuffle(helper_moves.as_mut_slice());
                let mut alpha = -i16::MAX;
                let mut search_data = SearchData::new(&helper_ks, &trans_table, doubles, hstx);

                for mv in helper_moves {
                    let board = board.clone_make(mv);
                    let score = -pvs(&board, -i16::MAX, i16::MAX, draft, 0, &mut search_data, Some(mv));

                    if helper_ks.load(SeqCst) { break; }

                    alpha = alpha.max(score);
                }
            });
            helper_threads.push(handle);
        }

        // main search thread
        let mut search_data = SearchData::new(&kill_switch, &trans_table, double_pos_rep.clone(), prev_hstx);

        let mut alpha = -i16::MAX;
        for &mv in root_moves.iter() {
            let board = board.clone_make(mv);
            let score = -pvs(&board, -i16::MAX, -alpha, draft, 0, &mut search_data, Some(mv));
            
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
    pub killers: [[Option<Move>; 2]; MAX_DEPTH],
    pub history: [[u64; 64]; 6],
}
impl Heuristics {
    pub fn new() -> Self {
        Self {
            killers: [[None;  2]; MAX_DEPTH],
            history: [[0u64; 64]; 6],
        }
    }
}

#[derive(Clone)]
struct SearchData<'a> {
    pub kill_switch: &'a AtomicBool,
    pub trans_table: &'a TransTable,

    pub double_pos_rep: HashSet<u64, U64IdentHashBuilder>,
    pub search_pos_rep: HashSet<u64, U64IdentHashBuilder>,
    /* pub pv: Vec<Option<Move>>, // make shared? somehow? */ 

    //pub node_count: usize,
    pub check_exts: u8,
    pub hstx: Heuristics,
}
impl<'a> SearchData<'a> {
    pub fn new(
        kill_switch: &'a AtomicBool,
        trans_table: &'a TransTable,
        double_pos_rep: HashSet<u64, U64IdentHashBuilder>,
        hstx: Heuristics,
    ) -> Self {
        Self {
            kill_switch,
            trans_table,

            double_pos_rep,
            search_pos_rep: HashSet::with_capacity_and_hasher(MAX_DEPTH, U64IdentHashBuilder),

            check_exts: 0,
            hstx,
        }
    }
    
    #[inline]
    pub fn is_rep(&self, hash: u64) -> bool {
        self.search_pos_rep.contains(&hash) || self.double_pos_rep.contains(&hash)
    }
}

fn pvs(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8, data: &mut SearchData, prev: Option<Move>) -> i16 {
    if draft <= 0 { return quesce(board, alpha, beta, draft, depth, data, prev.unwrap()); }
    if draft >= 3 && data.kill_switch.load(SeqCst) { return -i16::MAX; }

    let og_alpha = alpha;
    let mut pv = Option::<Move>::None;
    
    // transposition table
    // avoid transpositions near a fifty move rule adjudication
    if 100 - board.fifty_move_clock as i8 > draft {
        if let Some(search_node) = data.trans_table.get(board.hash) {
            match search_node {
                SearchNode::Score { score_kind: kind, score, draft: sn_draft, pv: sn_pv } => {
                    // we need to check that this pv doesn't turn out to be a repetition/draw
                    // if it does, we need to re-search
                    if !data.is_rep(board.make_hash(sn_pv)) {
                        if sn_draft >= draft {
                            // search is deeper, hence it is sufficient to be decisive
                            match kind {
                                ScoreKind::Exact => return score,
                                ScoreKind::LoBound => if score >= beta  { return score; },
                                ScoreKind::HiBound => if score <= alpha { return score; },
                            }
                        }
    
                        pv = Some(sn_pv);
                    }
                },
                SearchNode::Checkmate => return eval::MATE + depth as i16,
                SearchNode::Stalemate => return eval::DRAW,
            }
        }
    }

    let in_check = board.in_check();

    let mut check_ext = false;
    if in_check && draft <= 2 && data.check_exts < 1 {
        check_ext = true;
        data.check_exts += 1;
    }

    //let pos_eval = board.eval();
    
    // null move pruning
    if draft > 3 && !in_check && board.actv & !board.pawns & !board.actv_king.bm() != 0 {
        if board.eval() >= beta {
            let board = board.clone_make_null();
            let null_score = -pvs(&board, -beta, -alpha, draft - 3, depth + 1, data, None);
            if null_score >= beta { return beta; }
        }
    }

    // futility pruning prerequisites
    let futility = draft == 1 && alpha > board.eval() + 250 && !in_check && (alpha <= 30000 || alpha >= -30000);

    // alpha-beta search
    let mut best: Option<(i16, Move)> = None;
    let search = |mv: Move, board: &Board, data: &mut SearchData, is_boring: bool| -> bool {
        let board = board.clone_make(mv);

        // check if futility pruning can be applied
        if futility && best.is_some() && is_boring && !board.in_check() { return false; }

        let mut score;
        if data.is_rep(board.hash) || board.is_draw_50_move() || board.is_draw_insf_mtrl() {
            score = eval::DRAW;
        } else {
            data.search_pos_rep.insert(board.hash);

            let child_draft = draft - 1 + check_ext as i8 * 2;
            if best.is_none() || pv.is_none() && alpha == og_alpha {
                score = -pvs(&board, -beta, -alpha, child_draft, depth + 1, data, Some(mv));
            } else {
                score = -pvs(&board, -alpha-1, -alpha, child_draft, depth + 1, data, Some(mv));
                if alpha <= score && score < beta {
                    score = -pvs(&board, -beta, -alpha, child_draft, depth + 1, data, Some(mv));
                }
            }

            data.search_pos_rep.remove(&board.hash);
        }
        
        if best.map_or(true, |b| b.0 <= score) {
            best = Some((score, mv));
            if score > alpha { alpha = score; }
            if score >= beta { return true; }
        }

        false
    };
    
    // move generation and ordering
    let full = beta.saturating_sub(og_alpha) > 1 && draft > 2;
    ord::ord(search, board, depth, data, pv, prev, full);

    if check_ext { data.check_exts -= 1; }

    if let Some((score, mv)) = best {
        if board.all & mv.to.bm() == 0 {
            // update move ordering heuristics
            
            // update killer move
            let [k1, k2] = &mut data.hstx.killers[depth as usize];
            match k1 {
                Some(k1v) if *k1v == mv => (),
                _ => { *k2 = *k1; *k1 = Some(mv); },
            }

            let weight = (draft as u64).pow(2);
            // update history heuristic
            data.hstx.history[mv.piece as usize][mv.to.us()] += weight;
        }

        if score != eval::DRAW { // avoid storing draw scores due to repetitions
            // update transposition table
            
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
        }

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

fn quesce(board: &Board, mut alpha: i16, beta: i16, draft: i8, depth: u8, data: &mut SearchData, prev: Move) -> i16 {
    if draft <= MAX_QUESCE_DRAFT { return board.eval(); }

    let in_check = board.in_check();
    let mut max = -i16::MAX;

    if !in_check { // no pruning in check
        // stand pat
        max = board.eval(); 
        if max >= beta { return max; }
        if max > alpha { alpha = max; }

        // delta pruning
        let mut big_delta = 900;
        if board.pawns & board.actv & 0x00FF000000000000 != 0 { big_delta += 900; }
        if alpha > big_delta + max { return max; }
    }

    let mut quiet_position = true;
    let thing = |mv: Move, board: &Board| -> bool {
        quiet_position = false;

        let board = board.clone_make(mv);
        let score = -quesce(&board, -beta, -alpha, draft - 1, depth + 1, data, mv);

        if score > max {
            max = score;
            if score >= beta { return true; }
            if score > alpha { alpha = score; }
        }
        false
    };
    ord::ord_quesce(thing, board, in_check, prev);

    if quiet_position {
        if in_check {
            eval::UNCERTAIN_MATE + depth as i16
        } else {
            board.eval()
        }
    } else {
        max
    }
}
