use crate::{
    Board, Sq, for_sq, board::fend, search::htab::PawnKingEval, 
    KING_IDX as KING,
    QUEEN_IDX as QUEEN, 
    ROOK_IDX as ROOK,
    BISHOP_IDX as BISHOP,
    KNIGHT_IDX as KNIGHT,
    PAWN_IDX as PAWN,
};
use super::htab::PkEvalTable;


pub const DRAW: i16 = 0;
pub const MATE: i16 = -32000;
pub const UNCERTAIN_MATE: i16 = -31000;


#[inline]
fn popcnt(bb: u64) -> i16 {
    bb.count_ones() as i16
}

pub fn material_eval(board: &Board) -> i16 {
    let mut material = 0;
    material += (popcnt(board.pawns & board.actv)   - popcnt(board.pawns & board.idle)  ) * 100;
    material += (popcnt(board.knights & board.actv) - popcnt(board.knights & board.idle)) * 320;
    material += (popcnt(board.bishops & board.actv) - popcnt(board.bishops & board.idle)) * 330;
    material += (popcnt(board.rooks & board.actv)   - popcnt(board.rooks & board.idle)  ) * 500;
    material += (popcnt(board.queens & board.actv)  - popcnt(board.queens & board.idle) ) * 970;
    material
}



pub fn eval(pos: &Board, peht: &PkEvalTable) -> i16 {
    let mut eval = /* pesto(pos) as i32;// */infra(pos, peht) as i32;

    // awareness of upcoming draws to the 50 move rule
    eval = eval * 64.min(100 - pos.fifty_move_clock as i32) / 64;
    // small tempo bonus
    eval += 20;

    return eval as i16;
}

pub fn pesto(pos: &Board) -> i16 {
    let mut mg_eg = 0;
    let mut eval = [0, 0];

    let masks = [pos.actv, pos.idle];
    let pieces = [
        pos.actv_king.bm() | pos.idle_king.bm(), 
        pos.queens, pos.rooks, pos.bishops, pos.knights, pos.pawns
    ];

    for piece in KING..=PAWN {
        for side in [ACTV, IDLE] {
            for_sq!(sq in pieces[piece] & masks[side] => {
                mg_eg += PHASE_ADJUST[piece];
                for phase in [MG, EG] {
                    eval[phase] += PSQT[side][piece][sq.us()][phase];
                }
            });
        }
    }

    ((eval[MG] as i32 * mg_eg as i32 + eval[EG] as i32 * (24 - mg_eg as i32)) / 24) as i16
}

pub fn infra(pos: &Board, peht: &PkEvalTable) -> i16 {
    let mut eval = [0; 3];
    let mut phase = 0;
    let mask = [pos.actv, pos.idle];

    // major+minor piece-square tables & safe mobility
    let major_minor_data = [
        (pos.knights, KNIGHT, fend::knight_fend_wall as fn(Sq, u64) -> u64), 
        (pos.bishops, BISHOP, fend::bishop_fend      as fn(Sq, u64) -> u64), 
        (pos.rooks,   ROOK,   fend::rook_fend        as fn(Sq, u64) -> u64), 
        (pos.queens,  QUEEN,  fend::queen_fend       as fn(Sq, u64) -> u64), 
    ];
    let mut covered = [
        fend::pawns_fend_actv(pos.pawns & pos.actv),
        fend::pawns_fend_idle(pos.pawns & pos.idle),
    ];

    for (bb, idx, fend_fn) in major_minor_data {
        let mut temp_covered = [0, 0];
        for s in [ACTV, IDLE] {
            for_sq!(sq in bb & mask[s] => {
                phase += PHASE_ADJUST[idx];
                for p in [MG, EG] { eval[p] += PSQT[s][idx][sq.us()][p]; }
                
                let fend = fend_fn(sq, pos.all);
                let fend_bonus
                    = popcnt(fend & !covered[s] & !mask[s]) * MOBILITY
                    + popcnt(fend & pos.all) * FEND
                    + popcnt(fend) * COVERED;
                eval[INDI] += fend_bonus * SIGN[s];
                temp_covered[s] |= fend;
            });
        }
        covered[ACTV] |= temp_covered[ACTV];
        covered[IDLE] |= temp_covered[IDLE];
    }

    let misc_bonuses = misc_bonuses(pos);
    eval[0] += misc_bonuses[0]; eval[1] += misc_bonuses[1];


    // Pawn structure & king safety eval
    let mut pk_eval = [0; 2];
    /* if let Some(cached_pk_eval) = peht.get(pos.pk_hash) {
        pk_eval = cached_pk_eval.eval;
    } else {
        pk_eval = pawn_king(pos);
        peht.insert(pos.pk_hash, PawnKingEval { eval: pk_eval }, |_| true);
    } */

    let mg = (eval[MG] + pk_eval[MG]) as i32 * phase as i32;
    let eg = (eval[EG] + pk_eval[EG]) as i32 * (24 - phase as i32);

    eval[INDI] + ((mg + eg) / 24) as i16
}

fn density(pos: &Board, pcs: &[[i16; 2]; 5]) -> i16 {
    // calculate position openness heuristic
    // max 32 (realistically, 15 is very closed, 7 is very open)
    let op_cl = popcnt(pos.all & CENTRE) + popcnt(pos.pawns & CENTRE & (pos.pawns & CENTRE) << 0o10);
    let op_cl = (op_cl - 7).clamp(0, 8); // 0 is open, 8 is closed
    let mut op_cl_eval = [0, 0];

    // penalise knights in open positions and rooks in closed positions
    for p in [OP, CL] {
        op_cl_eval[p] += (pcs[KNIGHT][ACTV] - pcs[KNIGHT][IDLE]) * KNIGHT_OPEN_POS[p];
        op_cl_eval[p] += (pcs[ROOK][ACTV]   - pcs[IDLE][ACTV]  ) * ROOK_OPEN_POS[p];
    }

    (op_cl_eval[OP] * (8 - op_cl) + op_cl * op_cl_eval[CL]) / 8
}

fn misc_bonuses(pos: &Board) -> [i16; 2] {
    let mut eval = [0; 2];
    let mask = [pos.actv, pos.idle];

    // penalise bishops that face their own pawns
    for p in [MG, EG] {
        for c in [DARK, LIGHT] {
            for s in [ACTV, IDLE] {
                eval[p] 
                    += popcnt(mask[s] & pos.bishops & c) 
                    * (popcnt(mask[s] & pos.pawns   & c) - 2) 
                    * BISHOP_PAWN_SQ[p] * SIGN[s];
            }
        }
    }

    // favour bishop pairs
    let bishop_pair_favour 
        = (popcnt(pos.actv & pos.bishops) >= 2) as i16 
        - (popcnt(pos.idle & pos.bishops) >= 2) as i16;
    for p in [MG, EG] { eval[p] += bishop_pair_favour * BISHOP_PAIR[p]; }

    // favour rooks on open and half-open files in the middlegame
    for s in [ACTV, IDLE] {
        for_sq!(sq in pos.rooks & mask[s] => {
            let file_pawn_count = popcnt(FILE << sq.file() & pos.pawns);
            eval[MG] += file_pawn_count * MG_ROOK_PAWN_FILE * SIGN[s];
        });
    }

    eval
}

fn pawn_king(pos: &Board) -> [i16; 2] {
    let mut eval = [0, 0];
    let mask = [pos.actv, pos.idle];

    let king = [pos.actv_king, pos.idle_king];
    let king_ring = [fend::king_fend(king[ACTV]), fend::king_fend(king[IDLE])];
    /* // eval gets worse?
    mg_eval += popcnt(idle_cvrd & !actv_cvrd & actv_king_ring) * MG_KING_RING_FEND;
    mg_eval -= popcnt(actv_cvrd & !idle_cvrd & idle_king_ring) * MG_KING_RING_FEND; */


    // king safety + piece sq
    for p in [MG, EG] {
        for s in [ACTV, IDLE] {
            eval[p] += PSQT[s][KING][king[s].us()][p];
            eval[p] += popcnt(king_ring[s] & pos.pawns & mask[s]) * KING_RING_PAWN[p] * SIGN[s];
        }
    }

    // pawn structure
    for s in [ACTV, IDLE] {
        let pawns = pos.pawns & mask[s];
        for_sq!(sq in pawns => {
            let file = sq.file() as usize;
            for p in [MG, EG] { eval[p] += PSQT[s][PAWN][sq.us()][p]; }

            if popcnt(SUPPORT_MASKS[file] << ((sq.rank() - 1) * 8) & pawns) != 0 {
                for p in [MG, EG] { eval[p] += PAWN_SUPPORTED[p] * SIGN[s]; }
            }
            let passed_mask = match s {
                ACTV => PASSED_MASKS[s][file] << ((sq.rank()        - 1) * 8),
                IDLE => PASSED_MASKS[s][file] >> ((sq.flip().rank() - 1) * 8),
                _ => unreachable!(),
            };
            if passed_mask & pos.pawns & !mask[s] == 0 {
                for p in [MG, EG] { eval[p] += PAWN_PASSED[p] * SIGN[s]; }
            }
            if ISOLATED_MASKS[file] & pawns == 0 {
                for p in [MG, EG] { eval[p] += PAWN_ISOLATED[p] * SIGN[s]; }
            }
            if FILE << file & pawns & !sq.bm() != 0 {
                for p in [MG, EG] { eval[p] += PAWN_DOUBLED[p] * SIGN[s]; }
            }
        });
    }

    eval
}

const ACTV: usize = 0;
const IDLE: usize = 1;
const INDI: usize = 2;

const SIGN: [i16; 2] = [1, -1];

const CL: usize = 0;
const OP: usize = 1;

const MG: usize = 0;
const EG: usize = 1;

const CENTRE: u64 = 0x183C7E7E3C1800;
const FILE: u64 = 0x0101010101010101;
const LIGHT: u64 = 0x55AA55AA55AA55AA;
const DARK: u64 = 0xAA55AA55AA55AA55;


pub const MATERIAL: [[i16; 2]; 6] = [
    [0, 0], [970, 910], [490, 560], [330, 335], [340, 290], [100, 120],
];


const MOBILITY: i16 = 5; // safe mobility bonus
const COVERED: i16 = 2; // tile fend bonus
const FEND: i16 = 3; // occupied tile fend extra bonus



const MG_ROOK_PAWN_FILE: i16 = -20;
//const MG_KING_RING_FEND: i16 = -35;

// closed -> open
const KNIGHT_OPEN_POS: [i16; 2] = [ 40, -10];
const ROOK_OPEN_POS  : [i16; 2] = [-35,  25];

// middle -> end
const BISHOP_PAIR   : [i16; 2] = [ 15,  30];
const BISHOP_PAWN_SQ: [i16; 2] = [ -3,  -7];

const PAWN_DOUBLED  : [i16; 2] = [-16, -25];
const PAWN_ISOLATED : [i16; 2] = [-12,  -5];
const PAWN_SUPPORTED: [i16; 2] = [ 12,  20];
const PAWN_PASSED   : [i16; 2] = [ 30,  65];
const KING_RING_PAWN: [i16; 2] = [ 30,   8];


/* /// Middlegame and Endgame, for each square, for each piece, for actv and idle.
const PSQT: [[[[i16; 2]; 64]; 6]; 2] = {
    let mut psqt = [[[[0; 2]; 64]; 6]; 2];

    let mut r = 0;
    while r < 8 {
        let mut f = 0;
        while f < 4 {
            let mut p = 0;
            while p < 2 {
                let mut piece = 0;
                while piece < PAWN {
                    let eval = stockfish::PSQT[piece][r * 4 + f][p] + MATERIAL[piece][p];
                    psqt[ACTV][piece][     r * 8 +     f][p] =  eval;
                    psqt[ACTV][piece][     r * 8 + 7 - f][p] =  eval;
                    psqt[IDLE][piece][56 - r * 8 +     f][p] = -eval;
                    psqt[IDLE][piece][56 - r * 8 + 7 - f][p] = -eval;
                    piece += 1;
                }

                const PAWN_RANK_BONUS: [i16; 2] = [10, 15];
                let pawn_rank_multi = r.saturating_sub(3).pow(2) as i16; // 0, 0, 0, 0, 1, 4, 9
                let pawn_bonus = MATERIAL[PAWN][p] + pawn_rank_multi * PAWN_RANK_BONUS[p];
                psqt[ACTV][PAWN][     r * 8 +     f][p] =  stockfish::PAWN_PSQT[r * 8 +     f][p] + pawn_bonus;
                psqt[ACTV][PAWN][     r * 8 + 7 - f][p] =  stockfish::PAWN_PSQT[r * 8 + 7 - f][p] + pawn_bonus;
                psqt[IDLE][PAWN][56 - r * 8 +     f][p] = -stockfish::PAWN_PSQT[r * 8 +     f][p] - pawn_bonus;
                psqt[IDLE][PAWN][56 - r * 8 + 7 - f][p] = -stockfish::PAWN_PSQT[r * 8 + 7 - f][p] - pawn_bonus;

                p += 1;
            }
            f += 1;
        }
        r += 1;
    }

    psqt
}; */


/// Middlegame and Endgame, for each square, for each piece, for actv and idle.
const PSQT: [[[[i16; 2]; 64]; 6]; 2] = {
    let mut psqt = [[[[0; 2]; 64]; 6]; 2];

    let mut sq = 0;
    while sq < 64 {
        let mut phase = 0;
        while phase < 2 {
            let mut piece = 0;
            while piece <= PAWN {
                let eval = pesto::PSQT[phase][piece][sq ^ 0o70] + pesto::MTRL[phase][piece];
                psqt[ACTV][piece][sq       ][phase] =  eval;
                psqt[IDLE][piece][sq ^ 0o70][phase] = -eval;
                piece += 1;
            }
            phase += 1;
        }
        sq += 1;
    }

    psqt
};

const PHASE_ADJUST: [i16; 6] = [0, 4, 2, 1, 1, 0];



// Pawn masks by file
const SUPPORT_MASKS: [u64; 8] = [ // assumes rank 2
    0x20202, 0x50505, 0xa0a0a, 0x141414, 0x282828, 0x505050, 0xa0a0a0, 0x404040,
];
const PASSED_MASKS: [[u64; 8]; 2] = [
    // assumes rank 2
    [ 0x303030303030000, 0x707070707070000, 0xe0e0e0e0e0e0000, 0x1c1c1c1c1c1c0000,
      0x3838383838380000, 0x7070707070700000, 0xe0e0e0e0e0e00000, 0xc0c0c0c0c0c00000 ],
    // assumes rank 7
    [ 0x30303030303, 0x70707070707, 0xe0e0e0e0e0e, 0x1c1c1c1c1c1c,
      0x383838383838, 0x707070707070, 0xe0e0e0e0e0e0, 0xc0c0c0c0c0c0],
];
const ISOLATED_MASKS: [u64; 8] = [
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
];


mod stockfish {
    // Stockfish piece-square table values, ripped from here on 2022/07/05:
    // https://github.com/official-stockfish/Stockfish/blob/master/src/psqt.cpp

    /*
        Stockfish, a UCI chess playing engine derived from Glaurung 2.1

        Copyright (C) 2004-2022 The Stockfish developers (see AUTHORS file)
        
        Stockfish is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.
        
        Stockfish is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

    pub const PSQT: [[[i16; 2]; 32]; 5] = [
        [ // King
            [271,  1], [327, 45], [271, 85], [198, 76], 
            [278, 53], [303,100], [234,133], [179,135], 
            [195, 88], [258,130], [169,169], [120,175], 
            [164,103], [190,156], [138,172], [ 98,172], 
            [154, 96], [179,166], [105,199], [ 70,199], 
            [123, 92], [145,172], [ 81,184], [ 31,191], 
            [ 88, 47], [120,121], [ 65,116], [ 33,131], 
            [ 59, 11], [ 89, 59], [ 45, 73], [ -1, 78], 
        ],
        [ // Queen
            [ 3,-69], [-5,-57], [-5,-47], [ 4,-26], 
            [-3,-54], [ 5,-31], [ 8,-22], [12, -4], 
            [-3,-39], [ 6,-18], [13, -9], [ 7,  3], 
            [ 4,-23], [ 5, -3], [ 9, 13], [ 8, 24], 
            [ 0,-29], [14, -6], [12,  9], [ 5, 21], 
            [-4,-38], [10,-18], [ 6,-11], [ 8,  1], 
            [-5,-50], [ 6,-27], [10,-24], [ 8, -8], 
            [-2,-74], [-2,-52], [ 1,-43], [-2,-34], 
        ],
        [ // Rook
            [-31, -9], [-20,-13], [-14,-10], [-5, -9], 
            [-21,-12], [-13, -9], [ -8, -1], [ 6, -2], 
            [-25,  6], [-11, -8], [ -1, -2], [ 3, -6], 
            [-13, -6], [ -5,  1], [ -4, -9], [-6,  7], 
            [-27, -5], [-15,  8], [ -4,  7], [ 3, -6], 
            [-22,  6], [ -2,  1], [  6, -7], [12, 10], 
            [ -2,  4], [ 12,  5], [ 16, 20], [18, -5], 
            [-17, 18], [-19,  0], [ -1, 19], [ 9, 13], 
        ],
        [ // Bishop
            [-37,-40], [ -4,-21], [ -6,-26], [-16, -8], 
            [-11,-26], [  6, -9], [ 13,-12], [  3,  1], 
            [ -5,-11], [ 15, -1], [ -4, -1], [ 12,  7], 
            [ -4,-14], [  8, -4], [ 18,  0], [ 27, 12], 
            [ -8,-12], [ 20, -1], [ 15,-10], [ 22, 11], 
            [-11,-21], [  4,  4], [  1,  3], [  8,  4], 
            [-12,-22], [-10,-14], [  4, -1], [  0,  1], 
            [-34,-32], [  1,-29], [-10,-26], [-16,-17], 
        ],
        [ // Knight
            [-175, -96], [-92,-65], [-74,-49], [-73,-21], 
            [ -77, -67], [-41,-54], [-27,-18], [-15,  8], 
            [ -61, -40], [-17,-27], [  6, -8], [ 12, 29], 
            [ -35, -35], [  8, -2], [ 40, 13], [ 49, 28], 
            [ -34, -45], [ 13,-16], [ 44,  9], [ 51, 39], 
            [  -9, -51], [ 22,-44], [ 58,-16], [ 53, 17], 
            [ -67, -69], [-27,-50], [  4,-51], [ 37, 12], 
            [-201,-100], [-83,-88], [-56,-56], [-26,-17], 
        ],
    ];

    pub const PAWN_PSQT: [[i16; 2]; 64] = [
        [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], 
        [  2, -8], [  4, -6], [ 11,  9], [ 18,  5], [ 16, 16], [ 21,  6], [  9, -6], [ -3,-18],
        [ -9, -9], [-15, -7], [ 11,-10], [ 15,  5], [ 31,  2], [ 23,  3], [  6, -8], [-20, -5],
        [ -3,  7], [-20,  1], [  8, -8], [ 19, -2], [ 39,-14], [ 17,-13], [  2,-11], [ -5, -6],
        [ 11, 12], [ -4,  6], [-11,  2], [  2, -6], [ 11, -5], [  0, -4], [-12, 14], [  5,  9],
        [  3, 27], [-11, 18], [ -6, 19], [ 22, 29], [ -8, 30], [ -5,  9], [-14,  8], [-11, 14],
        [ -7, -1], [  6,-14], [ -2, 13], [-11, 22], [  4, 24], [-14, 17], [ 10,  7], [ -9,  7],
        [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], [  0,  0], 
    ];
}

#[cfg(test)]
mod tests {
    #[test]
    fn psqt_sym_test() {
        for rank in 0..8 {
            for file in 0..8 {
                for piece in 0..6 {
                    for phase in 0..2 {
                        assert_eq!(
                            super::PSQT[0][piece][rank * 8 + file][phase],
                            -super::PSQT[1][piece][56 - rank * 8 + file][phase],
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn eval_sym_test() {
        //  8/p2p4/r7/1k6/8/pK5Q/P7/b7 w - - 
        let board = crate::Board::from_fen("r4q1k/p2bR1rp/2p2Q1N/5p2/5p2/2P5/PP3PPP/R5K1 w - - ").unwrap();
        let mut flip = board.clone(); flip.flip();
        assert_eq!(
            super::eval(&board, &crate::search::htab::PkEvalTable::with_capacity(0)), 
            -super::eval(&flip, &crate::search::htab::PkEvalTable::with_capacity(0)), 
        );
    }
}

mod pesto {
    const MG_PAWN_TABLE: [i16; 64] = [
          0,   0,   0,   0,   0,   0,  0,   0,
         98, 134,  61,  95,  68, 126, 34, -11,
         -6,   7,  26,  31,  65,  56, 25, -20,
        -14,  13,   6,  21,  23,  12, 17, -23,
        -27,  -2,  -5,  12,  17,   6, 10, -25,
        -26,  -4,  -4, -10,   3,   3, 33, -12,
        -35,  -1, -20, -23, -15,  24, 38, -22,
          0,   0,   0,   0,   0,   0,  0,   0,
    ];
    
    const EG_PAWN_TABLE: [i16; 64] = [
          0,   0,   0,   0,   0,   0,   0,   0,
        178, 173, 158, 134, 147, 132, 165, 187,
         94, 100,  85,  67,  56,  53,  82,  84,
         32,  24,  13,   5,  -2,   4,  17,  17,
         13,   9,  -3,  -7,  -7,  -8,   3,  -1,
          4,   7,  -6,   1,   0,  -5,  -1,  -8,
         13,   8,   8,  10,  13,   0,   2,  -7,
          0,   0,   0,   0,   0,   0,   0,   0,
    ];
    
    const MG_KNIGHT_TABLE: [i16; 64] = [
        -167, -89, -34, -49,  61, -97, -15, -107,
         -73, -41,  72,  36,  23,  62,   7,  -17,
         -47,  60,  37,  65,  84, 129,  73,   44,
          -9,  17,  19,  53,  37,  69,  18,   22,
         -13,   4,  16,  13,  28,  19,  21,   -8,
         -23,  -9,  12,  10,  19,  17,  25,  -16,
         -29, -53, -12,  -3,  -1,  18, -14,  -19,
        -105, -21, -58, -33, -17, -28, -19,  -23,
    ];
    
    const EG_KNIGHT_TABLE: [i16; 64] = [
        -58, -38, -13, -28, -31, -27, -63, -99,
        -25,  -8, -25,  -2,  -9, -25, -24, -52,
        -24, -20,  10,   9,  -1,  -9, -19, -41,
        -17,   3,  22,  22,  22,  11,   8, -18,
        -18,  -6,  16,  25,  16,  17,   4, -18,
        -23,  -3,  -1,  15,  10,  -3, -20, -22,
        -42, -20, -10,  -5,  -2, -20, -23, -44,
        -29, -51, -23, -15, -22, -18, -50, -64,
    ];
    
    const MG_BISHOP_TABLE: [i16; 64] = [
        -29,   4, -82, -37, -25, -42,   7,  -8,
        -26,  16, -18, -13,  30,  59,  18, -47,
        -16,  37,  43,  40,  35,  50,  37,  -2,
         -4,   5,  19,  50,  37,  37,   7,  -2,
         -6,  13,  13,  26,  34,  12,  10,   4,
          0,  15,  15,  15,  14,  27,  18,  10,
          4,  15,  16,   0,   7,  21,  33,   1,
        -33,  -3, -14, -21, -13, -12, -39, -21,
    ];
    
    const EG_BISHOP_TABLE: [i16; 64] = [
        -14, -21, -11,  -8, -7,  -9, -17, -24,
         -8,  -4,   7, -12, -3, -13,  -4, -14,
          2,  -8,   0,  -1, -2,   6,   0,   4,
         -3,   9,  12,   9, 14,  10,   3,   2,
         -6,   3,  13,  19,  7,  10,  -3,  -9,
        -12,  -3,   8,  10, 13,   3,  -7, -15,
        -14, -18,  -7,  -1,  4,  -9, -15, -27,
        -23,  -9, -23,  -5, -9, -16,  -5, -17,
    ];
    
    const MG_ROOK_TABLE: [i16; 64] = [
         32,  42,  32,  51, 63,  9,  31,  43,
         27,  32,  58,  62, 80, 67,  26,  44,
         -5,  19,  26,  36, 17, 45,  61,  16,
        -24, -11,   7,  26, 24, 35,  -8, -20,
        -36, -26, -12,  -1,  9, -7,   6, -23,
        -45, -25, -16, -17,  3,  0,  -5, -33,
        -44, -16, -20,  -9, -1, 11,  -6, -71,
        -19, -13,   1,  17, 16,  7, -37, -26,
    ];
    
    const EG_ROOK_TABLE: [i16; 64] = [
        13, 10, 18, 15, 12,  12,   8,   5,
        11, 13, 13, 11, -3,   3,   8,   3,
         7,  7,  7,  5,  4,  -3,  -5,  -3,
         4,  3, 13,  1,  2,   1,  -1,   2,
         3,  5,  8,  4, -5,  -6,  -8, -11,
        -4,  0, -5, -1, -7, -12,  -8, -16,
        -6, -6,  0,  2, -9,  -9, -11,  -3,
        -9,  2,  3, -1, -5, -13,   4, -20,
    ];
    
    const MG_QUEEN_TABLE: [i16; 64] = [
        -28,   0,  29,  12,  59,  44,  43,  45,
        -24, -39,  -5,   1, -16,  57,  28,  54,
        -13, -17,   7,   8,  29,  56,  47,  57,
        -27, -27, -16, -16,  -1,  17,  -2,   1,
         -9, -26,  -9, -10,  -2,  -4,   3,  -3,
        -14,   2, -11,  -2,  -5,   2,  14,   5,
        -35,  -8,  11,   2,   8,  15,  -3,   1,
         -1, -18,  -9,  10, -15, -25, -31, -50,
    ];
    
    const EG_QUEEN_TABLE: [i16; 64] = [
         -9,  22,  22,  27,  27,  19,  10,  20,
        -17,  20,  32,  41,  58,  25,  30,   0,
        -20,   6,   9,  49,  47,  35,  19,   9,
          3,  22,  24,  45,  57,  40,  57,  36,
        -18,  28,  19,  47,  31,  34,  39,  23,
        -16, -27,  15,   6,   9,  17,  10,   5,
        -22, -23, -30, -16, -16, -23, -36, -32,
        -33, -28, -22, -43,  -5, -32, -20, -41,
    ];
    
    const MG_KING_TABLE: [i16; 64] = [
        -65,  23,  16, -15, -56, -34,   2,  13,
         29,  -1, -20,  -7,  -8,  -4, -38, -29,
         -9,  24,   2, -16, -20,   6,  22, -22,
        -17, -20, -12, -27, -30, -25, -14, -36,
        -49,  -1, -27, -39, -46, -44, -33, -51,
        -14, -14, -22, -46, -44, -30, -15, -27,
          1,   7,  -8, -64, -43, -16,   9,   8,
        -15,  36,  12, -54,   8, -28,  24,  14,
    ];
    
    const EG_KING_TABLE: [i16; 64] = [
        -74, -35, -18, -18, -11,  15,   4, -17,
        -12,  17,  14,  17,  17,  38,  23,  11,
         10,  17,  23,  15,  20,  45,  44,  13,
         -8,  22,  24,  27,  26,  33,  26,   3,
        -18,  -4,  21,  24,  27,  23,   9, -11,
        -19,  -3,  11,  21,  23,  16,   7,  -9,
        -27, -11,   4,  13,  14,   4,  -5, -17,
        -53, -34, -21, -11, -28, -14, -24, -43
    ];
    
    /// Note: tables are upside down.
    pub const PSQT: [[[i16; 64]; 6]; 2] = [
        [
            MG_KING_TABLE,
            MG_QUEEN_TABLE,
            MG_ROOK_TABLE,
            MG_BISHOP_TABLE,
            MG_KNIGHT_TABLE,
            MG_PAWN_TABLE,
        ],
        [
            EG_KING_TABLE,
            EG_QUEEN_TABLE,
            EG_ROOK_TABLE,
            EG_BISHOP_TABLE,
            EG_KNIGHT_TABLE,
            EG_PAWN_TABLE,
        ]
    ];

    const MG_MAT: [i16; 6] = [0, 1025, 466, 365, 337, 82];
    const EG_MAT: [i16; 6] = [0,  936, 512, 297, 281, 94];

    pub const MTRL: [[i16; 6]; 2] = [MG_MAT, EG_MAT];
    pub const PHASE_CONTR: [i16; 6] = [0, 4, 2, 1, 1, 0];
}
