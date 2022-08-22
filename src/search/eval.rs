use crate::{
    Board, Sq, for_sq, board::fend, search::htab::PawnKingEval, 
    QUEEN_IDX, ROOK_IDX, BISHOP_IDX, KNIGHT_IDX, PAWN_IDX, KING_IDX
};

use super::htab::PkEvalTable;

pub const DRAW: i16 = 0;
pub const MATE: i16 = -32000;
pub const UNCERTAIN_MATE: i16 = -31000;


#[inline]
fn popcnt(bb: u64) -> i16 {
    bb.count_ones() as i16
}

pub fn basic_mat_eval(board: &Board) -> i16 {
    let mut material = 0;
    material += (popcnt(board.pawns & board.actv)   - popcnt(board.pawns & board.idle)  ) * 100;
    material += (popcnt(board.knights & board.actv) - popcnt(board.knights & board.idle)) * 320;
    material += (popcnt(board.bishops & board.actv) - popcnt(board.bishops & board.idle)) * 330;
    material += (popcnt(board.rooks & board.actv)   - popcnt(board.rooks & board.idle)  ) * 500;
    material += (popcnt(board.queens & board.actv)  - popcnt(board.queens & board.idle) ) * 970;
    material
}


pub fn eval(pos: &Board, peht: &PkEvalTable) -> i16 {
    let mut phase_eval = 0i16;
    let mut mg_eval = 0i16;
    let mut eg_eval = 0i16;

    let actv_pawn_count   = popcnt(pos.pawns   & pos.actv);
    let actv_knight_count = popcnt(pos.knights & pos.actv);
    let actv_bishop_count = popcnt(pos.bishops & pos.actv);
    let actv_rook_count   = popcnt(pos.rooks   & pos.actv);
    let actv_queen_count  = popcnt(pos.queens  & pos.actv);

    let idle_pawn_count   = popcnt(pos.pawns   & pos.idle);
    let idle_knight_count = popcnt(pos.knights & pos.idle);
    let idle_bishop_count = popcnt(pos.bishops & pos.idle);
    let idle_rook_count   = popcnt(pos.rooks   & pos.idle);
    let idle_queen_count  = popcnt(pos.queens  & pos.idle);

    // Get a value based on material to indicate endgame progression (~1 is mg, ~0 is eg)
    // The typical output is <= 64 and minimum is zero (two kings and up to three pawns).
    // De-emphasises queen and pawn material (Large amounts thereof are typical of endgames.)
    // This value is used to interpolate between middlegame and endgame table values.
    let phase = (
            (actv_knight_count + idle_knight_count + actv_bishop_count + idle_bishop_count) * 3 +
            (actv_rook_count + idle_rook_count + actv_queen_count + idle_queen_count) * 5 +
            (actv_pawn_count + idle_pawn_count) / 3
        ) / 64;

    // tally up material
    mg_eval += (actv_queen_count  - idle_queen_count ) * PIECE_MATERIAL[QUEEN_IDX][MG];
    mg_eval += (actv_rook_count   - idle_rook_count  ) * PIECE_MATERIAL[ROOK_IDX][MG];
    mg_eval += (actv_bishop_count - idle_bishop_count) * PIECE_MATERIAL[BISHOP_IDX][MG];
    mg_eval += (actv_knight_count - idle_knight_count) * PIECE_MATERIAL[KNIGHT_IDX][MG];
    mg_eval += (actv_pawn_count   - idle_pawn_count  ) * PIECE_MATERIAL[PAWN_IDX][MG];

    eg_eval += (actv_queen_count  - idle_queen_count ) * PIECE_MATERIAL[QUEEN_IDX][EG];
    eg_eval += (actv_rook_count   - idle_rook_count  ) * PIECE_MATERIAL[ROOK_IDX][EG];
    eg_eval += (actv_bishop_count - idle_bishop_count) * PIECE_MATERIAL[BISHOP_IDX][EG];
    eg_eval += (actv_knight_count - idle_knight_count) * PIECE_MATERIAL[KNIGHT_IDX][EG];
    eg_eval += (actv_pawn_count   - idle_pawn_count  ) * PIECE_MATERIAL[PAWN_IDX][EG];
    
    // approximate some sense of openness vs closedness of the position (~1 is closed, ~0 is open)
    // max 32 (realistically, 15 is very closed, 7 is very open)
    const CENTRE: u64 = 0x183C7E7E3C1800;
    let op_cl = popcnt(pos.all & CENTRE) + popcnt(pos.pawns & CENTRE & pos.pawns << 0o10);
    let op_cl = (op_cl - 7).clamp(0, 8); // 0 is open, 10 is closed
    let mut op_eval = 0i16;
    let mut cl_eval = 0i16;

    // penalise knights in open positions and rooks in closed positions
    op_eval += (actv_knight_count - idle_knight_count) * KNIGHT_OPEN_POS[OP];
    cl_eval += (actv_knight_count - idle_knight_count) * KNIGHT_OPEN_POS[CL];
    op_eval += (actv_rook_count   - idle_rook_count  ) * ROOK_OPEN_POS[OP];
    cl_eval += (actv_rook_count   - idle_rook_count  ) * ROOK_OPEN_POS[CL];

    phase_eval += (op_eval * op_cl + (10 - op_cl) * cl_eval) / 8;


    // penalise bishops whose pawns occupy the same squares most (offset to avoid overly devaluing bishops)
    mg_eval += popcnt(pos.actv & pos.bishops & DARK ) * (popcnt(pos.actv & pos.pawns & DARK )-2) * BISHOP_PAWN_SQ[MG];
    mg_eval += popcnt(pos.actv & pos.bishops & LIGHT) * (popcnt(pos.actv & pos.pawns & LIGHT)-2) * BISHOP_PAWN_SQ[MG];
    mg_eval -= popcnt(pos.idle & pos.bishops & DARK ) * (popcnt(pos.idle & pos.pawns & DARK )-2) * BISHOP_PAWN_SQ[MG];
    mg_eval -= popcnt(pos.idle & pos.bishops & LIGHT) * (popcnt(pos.idle & pos.pawns & LIGHT)-2) * BISHOP_PAWN_SQ[MG];

    eg_eval += popcnt(pos.actv & pos.bishops & DARK ) * (popcnt(pos.actv & pos.pawns & DARK )-2) * BISHOP_PAWN_SQ[EG];
    eg_eval += popcnt(pos.actv & pos.bishops & LIGHT) * (popcnt(pos.actv & pos.pawns & LIGHT)-2) * BISHOP_PAWN_SQ[EG];
    eg_eval -= popcnt(pos.idle & pos.bishops & DARK ) * (popcnt(pos.idle & pos.pawns & DARK )-2) * BISHOP_PAWN_SQ[EG];
    eg_eval -= popcnt(pos.idle & pos.bishops & LIGHT) * (popcnt(pos.idle & pos.pawns & LIGHT)-2) * BISHOP_PAWN_SQ[EG];

    // favour bishop pairs
    let bishop_pair_favour = (actv_bishop_count >= 2) as i16 - (idle_bishop_count >= 2) as i16;
    mg_eval += bishop_pair_favour * BISHOP_PAIR[MG];
    eg_eval += bishop_pair_favour * BISHOP_PAIR[EG];


    // favour rooks on open and half-open files in the middlegame
    for_sq!(sq in pos.rooks & pos.actv => {
        let file_pawn_count = popcnt(FILE << (sq.file()) & pos.pawns);
        mg_eval += file_pawn_count * MG_ROOK_PAWN_FILE;
    });
    for_sq!(sq in pos.rooks & pos.idle => {
        let file_pawn_count = popcnt(FILE << (sq.file()) & pos.pawns);
        mg_eval -= file_pawn_count * MG_ROOK_PAWN_FILE;
    });


    // major+minor piece-square tables
    // safe mobility
    let ma_mi_piece_data = [
        (pos.knights, KNIGHT_IDX, fend::knight_fend_wall as fn(Sq, u64) -> u64), 
        (pos.bishops, BISHOP_IDX, fend::bishop_fend      as fn(Sq, u64) -> u64), 
        (pos.rooks,   ROOK_IDX,   fend::rook_fend        as fn(Sq, u64) -> u64), 
        (pos.queens,  QUEEN_IDX,  fend::queen_fend       as fn(Sq, u64) -> u64), 
    ];
    let mut actv_cvrd = fend::pawns_fend_actv(pos.pawns & pos.actv);
    let mut idle_cvrd = fend::pawns_fend_idle(pos.pawns & pos.idle);
    for (bb, idx, fend_fn) in ma_mi_piece_data {
        let mut temp_cvrd = 0;
        for_sq!(sq in bb & pos.actv => {
            mg_eval += stockfish::PSQTS[idx][sq.us()][MG];
            eg_eval += stockfish::PSQTS[idx][sq.us()][EG];
            let fend = fend_fn(sq, pos.all);
            temp_cvrd |= fend;
            phase_eval += popcnt(fend & !idle_cvrd & !pos.actv) * MOBILITY;
        });
        for_sq!(sq in bb & pos.idle => {
            mg_eval += stockfish::PSQTS[idx][sq.flip().us()][MG];
            eg_eval += stockfish::PSQTS[idx][sq.flip().us()][EG];
            let fend = fend_fn(sq, pos.all);
            idle_cvrd |= fend;
            phase_eval -= popcnt(fend & !actv_cvrd & !pos.idle) * MOBILITY;
        });
        actv_cvrd |= temp_cvrd;
    }


    // fend space & piece attack+defence
    phase_eval += (popcnt(actv_cvrd) - popcnt(idle_cvrd)) * COVERED;
    // rough heuristic; quescience searching should be doing most of this legwork
    phase_eval += (popcnt(actv_cvrd & pos.all) - popcnt(idle_cvrd & pos.all)) * FEND;


    let actv_king_ring = fend::king_fend(pos.actv_king)/*  &! (IDLE_PASSED_MASKS[actv_king%8] >> 7-actv_king/8) */;
    let idle_king_ring = fend::king_fend(pos.idle_king)/*  &! (ACTV_PASSED_MASKS[idle_king%8] << idle_king/8) */;
    
    // eval gets worse?
    //mg_eval += popcnt(idle_cvrd & !actv_cvrd & actv_king_ring) * MG_KING_RING_ATT;
    //mg_eval -= popcnt(actv_cvrd & !idle_cvrd & idle_king_ring) * MG_KING_RING_ATT;


    // Pawn structure & king safety eval

    let mut pk_eval = 0;
    // don't ignore large differences in phase
    let pk_phase_hash = pos.pk_hash * (phase as u64 >> 2);

    if let Some(cached_pk_eval) = peht.get(pk_phase_hash) {
        pk_eval = cached_pk_eval.eval;
    } else {
        let mut mg_pk_eval = 0i16;
        let mut eg_pk_eval = 0i16;

        // king safety + piece sq
        mg_pk_eval += stockfish::PSQTS[KING_IDX][pos.actv_king.us()][MG]; //pesto::MG_KING_PSQT[pos.actv_king.us()];
        eg_pk_eval += stockfish::PSQTS[KING_IDX][pos.actv_king.us()][EG]; //pesto::EG_KING_PSQT[pos.actv_king.us()];
        mg_pk_eval -= stockfish::PSQTS[KING_IDX][pos.idle_king.flip().us()][MG]; //pesto::MG_KING_PSQT[pos.idle_king.flip().us()];
        eg_pk_eval -= stockfish::PSQTS[KING_IDX][pos.idle_king.flip().us()][EG]; //pesto::EG_KING_PSQT[pos.idle_king.flip().us()];
        
        mg_pk_eval += popcnt(actv_king_ring & pos.pawns & pos.actv) * KING_RING_PAWN[MG];
        eg_pk_eval += popcnt(actv_king_ring & pos.pawns & pos.actv) * KING_RING_PAWN[EG];
        mg_pk_eval -= popcnt(idle_king_ring & pos.pawns & pos.idle) * KING_RING_PAWN[MG];
        eg_pk_eval -= popcnt(idle_king_ring & pos.pawns & pos.idle) * KING_RING_PAWN[EG];
    
        // pawn structure
        let actv_pawns = pos.pawns & pos.actv;
        let idle_pawns = pos.pawns & pos.idle;
        for_sq!(sq in actv_pawns => {
            let file = sq.file() as usize;
            mg_pk_eval += pesto::MG_PAWN_PSQT[sq.us()];
            eg_pk_eval += pesto::EG_PAWN_PSQT[sq.us()];
            if popcnt(SUPPORT_MASKS[file] << (sq.u8() - 0o10) & actv_pawns) != 0 {
                mg_pk_eval += PAWN_SUPPORTED[MG];
                eg_pk_eval += PAWN_SUPPORTED[EG];
            }
            if ACTV_PASSED_MASKS[file] << (sq.u8() - 0o10) & idle_pawns == 0 {
                mg_pk_eval += PAWN_PASSED[MG];
                eg_pk_eval += PAWN_PASSED[EG];
            }
            if ISOLATED_MASKS[file] & actv_pawns == 0 {
                mg_pk_eval += PAWN_ISOLATED[MG];
                eg_pk_eval += PAWN_ISOLATED[EG];
            }
            if FILE << file & actv_pawns & !sq.bm() != 0 {
                mg_pk_eval += PAWN_DOUBLED[MG];
                eg_pk_eval += PAWN_DOUBLED[EG];
            }
        });
        for_sq!(sq in idle_pawns => {
            let file = sq.file() as usize;
            mg_pk_eval -= pesto::MG_PAWN_PSQT[sq.flip().us()];
            eg_pk_eval -= pesto::EG_PAWN_PSQT[sq.flip().us()];
            if popcnt(SUPPORT_MASKS[file] << (sq.u8() - 0o10) & actv_pawns) != 0 {
                mg_pk_eval -= PAWN_SUPPORTED[MG];
                eg_pk_eval -= PAWN_SUPPORTED[EG];
            }
            if IDLE_PASSED_MASKS[file] >> (sq.flip().u8() - 0o10) & idle_pawns == 0 {
                mg_pk_eval -= PAWN_PASSED[MG];
                eg_pk_eval -= PAWN_PASSED[EG];
            }
            if ISOLATED_MASKS[file] & actv_pawns == 0 {
                mg_pk_eval -= PAWN_ISOLATED[MG];
                eg_pk_eval -= PAWN_ISOLATED[EG];
            }
            if FILE << file & actv_pawns & !sq.bm() != 0 {
                mg_pk_eval -= PAWN_DOUBLED[MG];
                eg_pk_eval -= PAWN_DOUBLED[EG];
            }
        });

        pk_eval += (mg_pk_eval * (24 - phase) + phase * eg_pk_eval) / 24;

        peht.insert(pk_phase_hash, PawnKingEval { eval: pk_eval }, |_| true);
    }

    phase_eval + pk_eval + ((mg_eval as i32 * (24 - phase as i32) + phase as i32 * eg_eval as i32) / 24) as i16
}

const FILE: u64 = 0x0101010101010101;
const LIGHT: u64 = 0x55AA55AA55AA55AA;
const DARK: u64 = 0xAA55AA55AA55AA55;

const OP: usize = 0;
const CL: usize = 1;

const MG: usize = 0;
const EG: usize = 1;

// Pawn masks by file
const SUPPORT_MASKS: [u64; 8] = [ // assumes rank 2
    0x20202,
    0x50505,
    0xa0a0a,
    0x141414,
    0x282828,
    0x505050,
    0xa0a0a0,
    0x404040,
];
const ACTV_PASSED_MASKS: [u64; 8] = [ // assumes rank 2
    0x303030303030000,
    0x707070707070000,
    0xe0e0e0e0e0e0000,
    0x1c1c1c1c1c1c0000,
    0x3838383838380000,
    0x7070707070700000,
    0xe0e0e0e0e0e00000,
    0xc0c0c0c0c0c00000,
];
const IDLE_PASSED_MASKS: [u64; 8] = [ // assumes rank 7
    0x30303030303,
    0x70707070707,
    0xe0e0e0e0e0e,
    0x1c1c1c1c1c1c,
    0x383838383838,
    0x707070707070,
    0xe0e0e0e0e0e0,
    0xc0c0c0c0c0c0,
];
const ISOLATED_MASKS: [u64; 8] = [
    0x202020202020202,
    0x505050505050505,
    0xa0a0a0a0a0a0a0a,
    0x1414141414141414,
    0x2828282828282828,
    0x5050505050505050,
    0xa0a0a0a0a0a0a0a0,
    0x4040404040404040,
];

// Piece material - indexed by Piece::{Piece} or {PIECE}_IDX
pub const PIECE_MATERIAL: [[i16; 2]; 6] = [
    [0, 0],
    [970, 920],
    [500, 570],
    [330, 335],
    [340, 290],
    [100, 120],
];


const MOBILITY: i16 = 5; // safe mobility bonus
const COVERED: i16 = 3; // tile fend bonus
const FEND: i16 = 4; // occupied tile fend extra bonus

const MG_ROOK_PAWN_FILE: i16 = -20;
//const MG_KING_RING_ATT : i16 = -35;

// ~1: closed -> ~0: open
const KNIGHT_OPEN_POS: [i16; 2] =  [60, -15];
const ROOK_OPEN_POS  : [i16; 2] = [-50,  40];

// ~1: middle -> ~0: end
const BISHOP_PAIR   : [i16; 2] = [ 15,  30];
const BISHOP_PAWN_SQ: [i16; 2] = [ -3,  -8];

const PAWN_DOUBLED  : [i16; 2] = [-16, -25];
const PAWN_ISOLATED : [i16; 2] = [-12,  -5];
const PAWN_SUPPORTED: [i16; 2] = [ 12,  20];
const PAWN_PASSED   : [i16; 2] = [ 30,  65];
const KING_RING_PAWN: [i16; 2] = [ 30,   8];


mod pesto {
    //! Piece-Square and Material PeSTO evaluation pub constants.
    

    // translated from here, because my domain-specific knowledge isn't sufficient:
    // https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function

    // piece/sq PSQTs
    // values from Rofchade: http://www.talkchess.com/forum3/viewtopic.php?f=2&t=68311&start=19

    pub const MG_PAWN_PSQT: [i16; 64] = [
          0,   0,   0,   0,   0,   0,  0,   0,
         98, 134,  61,  95,  68, 126, 34, -11,
         -6,   7,  26,  31,  65,  56, 25, -20,
        -14,  13,   6,  21,  23,  12, 17, -23,
        -27,  -2,  -5,  12,  17,   6, 10, -25,
        -26,  -4,  -4, -10,   3,   3, 33, -12,
        -35,  -1, -20, -23, -15,  24, 38, -22,
          0,   0,   0,   0,   0,   0,  0,   0,
    ];
    
    pub const EG_PAWN_PSQT: [i16; 64] = [
          0,   0,   0,   0,   0,   0,   0,   0,
        178, 173, 158, 134, 147, 132, 165, 187,
         94, 100,  85,  67,  56,  53,  82,  84,
         32,  24,  13,   5,  -2,   4,  17,  17,
         13,   9,  -3,  -7,  -7,  -8,   3,  -1,
          4,   7,  -6,   1,   0,  -5,  -1,  -8,
         13,   8,   8,  10,  13,   0,   2,  -7,
          0,   0,   0,   0,   0,   0,   0,   0,
    ];
}

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

    const STOCKFISH_PIECE_SQUARE_TABLE: [[[i16; 2]; 32]; 5] = [
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

    pub const PSQTS: [[[i16; 2]; 64]; 5] = {
        let mut psqts = [[[0; 2]; 64]; 5];

        let mut p = 0;
        while p < 5 {
            let mut r = 0;
            while r < 8 {
                let mut f = 0;
                while f < 4 {
                    let mut g = 0;
                    while g < 2 {
                        psqts[p][r * 8 +     f][g] = STOCKFISH_PIECE_SQUARE_TABLE[p][r * 4 + f][g];
                        psqts[p][r * 8 + 7 - f][g] = STOCKFISH_PIECE_SQUARE_TABLE[p][r * 4 + f][g];
                        g += 1;
                    }
                    f += 1;
                }
                r += 1;
            }
            p += 1;
        }

        psqts
    };
}

