use crate::{for_sq, Board, Move, board::fend, Piece};

use super::SearchData;



pub fn gen<'m, 'd, 'b, F>(mut f: F, board: &'b Board, depth: u8, data: &'d mut SearchData, pv: Option<Move>)
where F: FnMut(Move, &Board, &mut SearchData) -> bool {

    // Map of prior issued moves: from-to butterfly
    let mut already_moved = [0u64; 64];

    // PV move
    if let Some(pv) = pv {
        if f(pv, board, data) { return; }
        already_moved[pv.from_sq as usize] |= 1 << pv.to_sq;
    }

    /* //countermove
    if let Some(counter) = data.counters[prev_move.piece as usize][prev_move.to_sq as usize] {
        if board.is_valid_move(counter) && board.is_move_legal(counter) {
            if already_moved[counter.from_sq as usize] & 1 << counter.to_sq == 0 {
                if f(counter, board, data) { return; }
                already_moved[counter.from_sq as usize] |= 1 << counter.to_sq;
            }
        }
    } */

    // Killer moves
    if data.killers.len() > depth as usize {
        let (k1, k2) = data.killers[depth as usize];
        for k in [k1, k2] {
            if let Some(k) = k {
                if board.is_valid(k) {
                    if already_moved[k.from_sq as usize] & 1 << k.to_sq == 0 {
                        if f(k, board, data) { return; }
                        already_moved[k.from_sq as usize] |= 1 << k.to_sq;
                    }
                }
            }
        }
    } else {
        data.killers.push((None, None));
    }


    let mvv_lva = [
        (board.queens,  Piece::Queen,  fend::queen_fend       as fn(u8, u64) -> u64), 
        (board.rooks,   Piece::Rook,   fend::rook_fend        as fn(u8, u64) -> u64), 
        (board.bishops, Piece::Bishop, fend::bishop_fend      as fn(u8, u64) -> u64), 
        (board.knights, Piece::Knight, fend::knight_fend_wall as fn(u8, u64) -> u64), 
        (board.pawns & 0xFFFFFFFFFF00, Piece::Pawn, fend::pawn_fend_idle_wall as fn(u8, u64) -> u64), 
    ];

    /* // recapture on last move where possible
    let idle_to_sq = crate::board::flip_sq(prev_move.to_sq);
    for lva in (0..mvv_lva.len()).rev() {
        let (lvas, p, fend_fn) = mvv_lva[lva];
        for_sq!(asq in fend_fn(idle_to_sq, board.all) & lvas & board.actv => {
            if already_moved[asq as usize] & 1 << idle_to_sq == 0 {
                let mov = Move::new(asq, idle_to_sq, p);
                if board.is_move_legal(mov) && f(mov, board, data) { return; }
                already_moved[asq as usize] |= 1 << idle_to_sq;
            }
        });
    } */

    // MVV-LVA
    for mvv in 0..mvv_lva.len() {
        let mvv_bb = mvv_lva[mvv].0;
        for_sq!(vsq in mvv_bb & board.idle => {
            for lva in (0..mvv_lva.len()).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(vsq, board.all) & lvas & board.actv => {
                    if already_moved[asq as usize] & 1 << vsq == 0 {
                        let mov = Move::new(asq, vsq, p);
                        if board.is_legal(mov) && f(mov, board, data) { return; }
                        already_moved[asq as usize] |= 1 << vsq;
                    }
                });
            }
        });
    }

    board.for_mov(|mov| {
        already_moved[mov.from_sq as usize] & 1 << mov.to_sq == 0
        && f(mov, board, data)
    });

    //let move_buff_acme = data.move_buff.len(); 

    //let move_buff_slice = &mut data.move_buff[move_buff_base..move_buff_acme];
    //move_buff_slice.sort_unstable_by_key(|m| u16::MAX - data.history[m.piece as usize][m.to_sq as usize]);
    // shell sort at a single level to approach a descending sort cheaply
    /* const SORT_GAP: usize = 3;
    for i in SORT_GAP..move_buff_slice.len() {
        let temp = move_buff_slice[i];
        let temp_history = data.history[temp.piece as usize][temp.to_sq as usize];
        let mut j = i;
        loop {
            let mov = move_buff_slice[j - SORT_GAP];
            let history = data.history[mov.piece as usize][mov.to_sq as usize];
            if temp_history > history {
                move_buff_slice[j] = mov;
                j -= SORT_GAP;
            } else {
                break;
            }
            if j < SORT_GAP {
                break;
            }
        }
        move_buff_slice[j] = temp;
    }
    
    for i in move_buff_base..move_buff_acme {
        if f(data.move_buff[i], board, data) { return; }
    } */
}



pub fn gen_light<'m, 'd, 'b, F>(mut f: F, board: &'b Board, depth: u8, data: &'d mut SearchData, pv: Option<Move>)
where F: FnMut(Move, &Board, &mut SearchData) -> bool {

    // Map of prior issued moves: from-to butterfly
    let mut already_moved = [0u64; 64];

    // PV move
    if let Some(pv) = pv {
        if f(pv, board, data) { return; }
        already_moved[pv.from_sq as usize] |= 1 << pv.to_sq;
    }

    let mvv_lva = [
        (board.queens,  Piece::Queen,  fend::queen_fend          as fn(u8, u64) -> u64), 
        (board.rooks,   Piece::Rook,   fend::rook_fend           as fn(u8, u64) -> u64), 
        (board.bishops, Piece::Bishop, fend::bishop_fend         as fn(u8, u64) -> u64), 
        (board.knights, Piece::Knight, fend::knight_fend_wall    as fn(u8, u64) -> u64), 
        (board.pawns & 0xFFFFFFFFFF00, Piece::Pawn, fend::pawn_fend_idle_wall as fn(u8, u64) -> u64), 
    ];

    // MVV-LVA
    for mvv in 0..mvv_lva.len() {
        let mvv_bb = mvv_lva[mvv].0;
        for_sq!(vsq in mvv_bb & board.idle => {
            for lva in (mvv..mvv_lva.len()).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(vsq, board.all) & lvas & board.actv => {
                    if already_moved[asq as usize] & 1 << vsq == 0 {
                        let mov = Move::new(asq, vsq, p);
                        if board.is_legal(mov) && f(mov, board, data) { return; }
                        already_moved[asq as usize] |= 1 << vsq;
                    }
                });
            }
        });
    }
    
    // Killer moves
    if data.killers.len() > depth as usize {
        let (k1, k2) = data.killers[depth as usize];
        for k in [k1, k2] {
            if let Some(k) = k {
                if board.is_valid(k) {
                    if already_moved[k.from_sq as usize] & 1 << k.to_sq == 0 {
                        if f(k, board, data) { return; }
                        already_moved[k.from_sq as usize] |= 1 << k.to_sq;
                    }
                }
            }
        }
    } else {
        data.killers.push((None, None));
    }

    board.for_mov(|mov| {
        already_moved[mov.from_sq as usize] & 1 << mov.to_sq == 0
        && f(mov, board, data)
    });
}

#[inline]
pub fn gen_quesce<'m, 'd, 'b, F>(mut f: F, board: &'b Board, is_actv_in_check: bool)
where F: FnMut(Move, &Board) -> bool {
    
    if is_actv_in_check {
        board.for_mov(|mov| { f(mov, board) });
        return;
    }

    // MVV-LVA
    let mvv_lva = [
        (board.queens,  Piece::Queen,  fend::queen_fend          as fn(u8, u64) -> u64), 
        (board.rooks,   Piece::Rook,   fend::rook_fend           as fn(u8, u64) -> u64), 
        (board.bishops, Piece::Bishop, fend::bishop_fend         as fn(u8, u64) -> u64), 
        (board.knights, Piece::Knight, fend::knight_fend_wall    as fn(u8, u64) -> u64), 
        (board.pawns & 0xFFFFFFFFFF00, Piece::Pawn, fend::pawn_fend_idle_wall as fn(u8, u64) -> u64), 
    ];
    for mvv in 0..mvv_lva.len() {
        let mvv_bb = mvv_lva[mvv].0;
        for_sq!(vsq in mvv_bb & board.idle => {
            for lva in (mvv.saturating_sub(1)..mvv_lva.len()).rev() {
                let (lvas, p, fend_fn) = mvv_lva[lva];
                for_sq!(asq in fend_fn(vsq, board.all) & lvas & board.actv => {
                    let mov = Move::new(asq, vsq, p);
                    if board.is_legal(mov) && f(mov, board) { return; }
                });
            }
        });
    }
}
