use std::{sync::{Arc, atomic::AtomicBool}, thread::JoinHandle};

use crossbeam_channel::{Sender, Receiver};

pub use crate::{
    Board, Move, GameOver, 
    opening, board, search,
    board::zobrist::{PosHashMap, U64IdentHashBuilder},
    search::{SearchInfo, SearchEval, time::TimeControl},
    search::htab::{TransTable, self, PkEvalTable},
};



#[derive(Debug, Clone)]
pub struct Game {
    /// Game beginning position.
    pub begin_pos: Board,
    /// Current position.
    pub position: Board,
    /// Moves between `startpos` and `position`.
    pub move_list: Vec<Move>,
    /// Position hashes between `startpos` and `position`.
    pub prev_hashes: PosHashMap,
    /// Current distance from last book move, if known.
    pub book_distance: Option<usize>,
}

impl Game {
    fn update_hash_data(book_dist: &mut Option<usize>, prev_hashes: &mut PosHashMap, hash: u64) {
        // Track book distance
        if opening::query_book(hash).is_some() {
            *book_dist = Some(0);
        } else if let Some(dist) = *book_dist {
            *book_dist = Some(dist + 1);
        }

        // Add position hash
        prev_hashes.entry(hash).and_modify(|c| *c += 1).or_insert(0);
    }

    /// Create a chess game!
    /// 
    /// Returns `Err` variant when a move is invalid, returning the relevant position and move.
    pub fn new(begin_pos: Board, move_list: Vec<Move>) -> Result<Self, (Board, Move)> {
        Self::with_coded(begin_pos, move_list, |m, _| *m)
    }
    
    /// Create a chess game, given another move coding.
    /// 
    /// Returns `Err` variant when a move is invalid, returning the relevant position and move.
    pub fn with_coded<T, F>(begin_pos: Board, move_list: Vec<T>, mut f: F) -> Result<Self, (Board, Move)>
    where F: FnMut(&T, &Board) -> Move {
        let mut position = begin_pos.clone();
        let mut prev_hashes = PosHashMap::with_hasher(U64IdentHashBuilder);
        let mut book_distance = None;
        let mut decoded_move_list = Vec::with_capacity(move_list.len());

        for i in 0..=move_list.len() {
            Self::update_hash_data(&mut book_distance, &mut prev_hashes, position.hash);

            if i == move_list.len() { break; }

            // Make move
            let mv = f(&move_list[i], &position);
            decoded_move_list.push(mv);

            if position.is_valid(mv) {
                position.make(mv);
            } else {
                return Err((position, mv));
            }
        }

        position.validate().unwrap();

        Ok(Self {
            begin_pos,
            position,
            move_list: decoded_move_list,
            prev_hashes,
            book_distance,
        })
    }

    /// Play a move.
    pub fn play(&mut self, mv: Move) -> Result<Option<GameOver>, ()> {
        // Ensure move validity
        if !self.position.is_valid(mv) { return Err(()) }

        self.position.make(mv);
        self.move_list.push(mv);
        Self::update_hash_data(
            &mut self.book_distance, 
            &mut self.prev_hashes, 
            self.position.hash
        );

        // Return gameover status
        Ok(self.is_game_over())
    }

    /// Get a list of legal moves.
    pub fn get_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        self.position.for_move(|mv| { moves.push(mv); false });
        moves
    }

    /// Check if the game is over.
    pub fn is_game_over(&self) -> Option<GameOver> {
        self.position.is_game_over().or_else(|| 
            board::zobrist::threefold_repetition(&self.prev_hashes, self.position.hash)
                .then_some(GameOver::ThreefoldRepetition)
        )
    }


    pub fn search(&self,
        time_control: TimeControl, 
        trans_table: Option<Arc<TransTable>>,
        pk_eval_table: Option<Arc<PkEvalTable>>,
    ) -> SearchHandle {
        let kill_switch = Arc::new(AtomicBool::new(false));
        let (sndr, rcvr) = crossbeam_channel::unbounded();

        let thread_game = self.clone();
        let thread_kill_switch = kill_switch.clone();
        let thread_sndr = sndr.clone();

        let thread_handle = std::thread::spawn(move || {
            search::search(
                thread_game.position,
                thread_game.prev_hashes,
                thread_game.move_list.last().map(|&m| m),
                thread_sndr,
                thread_kill_switch,
                time_control.allocate_time(thread_game.book_distance),
                trans_table.unwrap_or(Arc::new(TransTable::with_memory(htab::TRANS_MEM_DEFAULT))),
                pk_eval_table.unwrap_or(Arc::new(PkEvalTable::with_memory(htab::PK_EVAL_MEM_DEFAULT))),
            )
        });

        SearchHandle { info_channel: (sndr, rcvr), kill_switch, thread_handle }
    }
}

#[derive(Debug)]
pub struct SearchHandle {
    /// Channel through which search data is sent.
    pub info_channel: (Sender<(SearchInfo, bool)>, Receiver<(SearchInfo, bool)>),

    /// Set the switch to kill the engine at any time (do not clear).
    /// 
    /// Note that killing will trigger a final evaluation to be emitted soon after.
    pub kill_switch: Arc<AtomicBool>,
    
    /// Handle to the engine thread.
    pub thread_handle: JoinHandle<()>,
}
