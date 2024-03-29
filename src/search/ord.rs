use std::mem::MaybeUninit;

use crate::{for_sq, Board, Move, board::fend, Piece, Sq};

use super::SearchData;

/// A fast, inline buffer for `Move` generation and ordering.
/// 
/// Only guaranteed to be safe in legal standard chess positions,
/// where no duplicate moves are added.
pub struct MoveBuffer {
    len: usize,
    // standard chess positions cannot legally have more than 108 moves or so
    buff: [MaybeUninit<Move>; 112],
}
impl MoveBuffer {
    pub fn new() -> Self {
        Self { len: 0, buff: [MaybeUninit::uninit(); 112] }
    }

    #[inline]
    pub fn push(&mut self, mv: Move) {
        debug_assert!(self.len < 112);
        unsafe {
            *self.buff.get_unchecked_mut(self.len) = MaybeUninit::new(mv);
        }
        self.len += 1;
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [Move] {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.buff.as_mut_ptr().cast::<Move>(),
                self.len, 
            )
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.len = 0;
    }
}

pub struct Bufferfly([u64; 64]);
impl Bufferfly {
    #[inline]
    pub fn new() -> Self { Self([0; 64]) }

    #[inline]
    pub fn set(&mut self, mv: Move) {
        self.0[mv.from.us()] |= mv.to.bm();
    }
    #[inline]
    pub fn get(&mut self, mv: Move) -> bool {
        self.0[mv.from.us()] & mv.to.bm() != 0
    }

    #[inline]
    pub fn is_novel(&mut self, mv: Move) -> bool {
        if !self.get(mv) { self.set(mv); true } else { false }
    }

    pub fn dbfly_reset(&mut self, mv: Move) -> bool {
        if self.get(mv) { self.0[mv.from.us()] &= !mv.to.bm(); true } else { false }
    }
}

#[inline]
const fn mvv_lva(board: &Board) -> [(u64, Piece, fn(Sq, u64) -> u64); 5] {
    [
        (board.queens,  Piece::Queen,  fend::queen_fend          as fn(Sq, u64) -> u64), 
        (board.rooks,   Piece::Rook,   fend::rook_fend           as fn(Sq, u64) -> u64), 
        (board.bishops, Piece::Bishop, fend::bishop_fend         as fn(Sq, u64) -> u64), 
        (board.knights, Piece::Knight, fend::knight_fend_wall    as fn(Sq, u64) -> u64), 
        (board.pawns,   Piece::Pawn,   fend::pawn_fend_idle_wall as fn(Sq, u64) -> u64), 
    ]
}

pub(super) fn ord<'d, 'b, F>(mut f: F, board: &'b Board, depth: u8, data: &'d mut SearchData, pv: Option<Move>, prev: Option<Move>, full: bool)
where F: FnMut(Move, &Board, &mut SearchData, bool) -> bool {

    // Map of prior issued moves: from-to butterfly
    let mut bfly = Bufferfly::new();

    // PV move
    if let Some(pv) = pv {
        if f(pv, board, data, false) { return; }
        bfly.set(pv);
    }

    let mvv_lva = mvv_lva(board);

    if !full {
        // recapture on last move
        if let Some(prev) = prev {
            let idle = prev.to.flip();
            for lva in (0..(mvv_lva.len()-1)).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(idle, board.all) & lvas & board.actv => {
                    let mv = Move::new(asq, idle, p);
                    if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board, data, false) { return; }
                });
            }
        }
    }

    // pawn promotions
    let mut mvbf = MoveBuffer::new();
    board.gen_proms(&mut mvbf);
    for &mut mv in mvbf.as_mut_slice() {
        if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board, data, false) { return; }
    }
    mvbf.clear();

    // pawn captures
    board.pawn_caps(&mut mvbf);
    for &mut mv in mvbf.as_mut_slice() {
        if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board, data, false) { return; }
    }
    mvbf.clear();

    // MVV-LVA - equal and winning
    for mvv in 0..(mvv_lva.len()-1) {
        let mvv_bb = mvv_lva[mvv].0;
        for_sq!(vsq in mvv_bb & board.idle => {
            for lva in (mvv..(mvv_lva.len()-1)).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(vsq, board.all) & lvas & board.actv => {
                    let mv = Move::new(asq, vsq, p);
                    if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board, data, false) { return; }
                });
            }
        });
    }

    // Killer moves
    for km in data.hstx.killers[depth as usize] {
        if let Some(km) = km {
            if !bfly.get(km) {
                if board.is_valid(km) {
                    if f(km, board, data, false) { return; }
                    bfly.set(km);
                }
            }
        }
    }

    // MVV-LVA - losing
    for mvv in 0..mvv_lva.len() {
        let mvv_bb = mvv_lva[mvv].0;
        for_sq!(vsq in mvv_bb & board.idle => {
            for lva in (0..mvv).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(vsq, board.all) & lvas & board.actv => {
                    let mv = Move::new(asq, vsq, p);
                    if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board, data, false) { return; }
                });
            }
        });
    }

    if !full {
        // remaining moves, generated on demand and unordered
        board.for_move(|mv| !bfly.get(mv) && f(mv, board, data, true));
    } else {
        // remaining moves, generated in bulk and ordered lazily

        board.gen_ncaps(&mut mvbf);
        let move_buff = mvbf.as_mut_slice();
        
        const SCAN_GAP: usize = 2; 
        for i in 0..move_buff.len() {
            let mut min = i;
    
            if i + 1 < move_buff.len() {
                let mut min_score = data.hstx.history[move_buff[min].piece as usize][move_buff[min].to.us()];
    
                for j in ((i+1)..move_buff.len()).step_by(SCAN_GAP) {
                    let mv = move_buff[j];
                    let score = data.hstx.history[mv.piece as usize][mv.to.us()];
                    if score < min_score {
                        min_score = score;
                        min = j;
                    }
                }
            }
            let mv = move_buff[min];
            move_buff[min] = move_buff[i];
            if !bfly.get(mv) && board.is_legal(mv) && f(mv, board, data, true) { return; }
        }
    }
}


#[inline]
pub fn ord_quesce<'d, 'b, F>(mut f: F, board: &'b Board, in_check: bool, prev_move: Move)
where F: FnMut(Move, &Board) -> bool {
    
    if in_check {
        board.for_move(|mv| f(mv, board));
        return;
    }

    let mut bfly = Bufferfly::new();
    let mvv_lva = mvv_lva(board);

    // recapture on last move where possible
    let prev_to = prev_move.to.flip();
    for lva in (0..(mvv_lva.len()-1)).rev() {
        let (lvas, p, fend_fn) = mvv_lva[lva];
        for_sq!(asq in fend_fn(prev_to, board.all) & lvas & board.actv => {
            let mv = Move::new(asq, prev_to, p);
            if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board) { return; }
        });
    }

    // pawn promotions
    let mut mvbf = MoveBuffer::new();
    board.gen_proms(&mut mvbf);
    for &mut mv in mvbf.as_mut_slice() {
        if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board) { return; }
    }
    mvbf.clear();

    // pawn captures
    board.pawn_caps(&mut mvbf);
    for &mut mv in mvbf.as_mut_slice() {
        if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board) { return; }
    }
    mvbf.clear();

    // MVV-LVA - equalish and winning
    for mvv in 0..(mvv_lva.len()-1) {
        let mvv_bb = mvv_lva[mvv].0;
        for_sq!(vsq in mvv_bb & board.idle => {
            for lva in (mvv.saturating_sub(1)..(mvv_lva.len()-1)).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(vsq, board.all) & lvas & board.actv => {
                    let mv = Move::new(asq, vsq, p);
                    if bfly.is_novel(mv) && board.is_legal(mv) && f(mv, board) { return; }
                });
            }
        });
    }
}
