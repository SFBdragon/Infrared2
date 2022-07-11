use crate::{Board, Piece, for_sq, board::fend};


pub const DRAW: i16 = 0;
pub const MATE: i16 = -32000;

pub fn basic_mat_eval(board: &Board) -> i16 {
    let mut material = 0;
    material += ((board.pawns & board.actv)  .count_ones() as i16 - (board.pawns & board.idle)  .count_ones() as i16) * 100;
    material += ((board.knights & board.actv).count_ones() as i16 - (board.knights & board.idle).count_ones() as i16) * 320;
    material += ((board.bishops & board.actv).count_ones() as i16 - (board.bishops & board.idle).count_ones() as i16) * 330;
    material += ((board.rooks & board.actv)  .count_ones() as i16 - (board.rooks & board.idle)  .count_ones() as i16) * 500;
    material += ((board.queens & board.actv) .count_ones() as i16 - (board.queens & board.idle) .count_ones() as i16) * 970;
    material
}


#[inline]
pub fn lerp(v: [f32; 2], mg_eg: f32) -> f32 {
    ( v[0] * (1.0 - mg_eg) ) + ( v[1] * mg_eg )
}
#[inline]
fn fold_file(mut file: usize) -> usize {
    if file & 0o4 != 0 {
        file ^= 0o7;
    }
    file
}
#[inline]
fn popcntf(bb: u64) -> f32 {
    bb.count_ones() as f32
}


pub fn eval(pos: &Board) -> i16 {
    let mut eval = 0.0f32;

    let actv_pawn_count   = popcntf(pos.pawns   & pos.actv);
    let actv_knight_count = popcntf(pos.knights & pos.actv);
    let actv_bishop_count = popcntf(pos.bishops & pos.actv);
    let actv_rook_count   = popcntf(pos.rooks   & pos.actv);
    let actv_queen_count  = popcntf(pos.queens  & pos.actv);

    let idle_pawn_count   = popcntf(pos.pawns   & pos.idle);
    let idle_knight_count = popcntf(pos.knights & pos.idle);
    let idle_bishop_count = popcntf(pos.bishops & pos.idle);
    let idle_rook_count   = popcntf(pos.rooks   & pos.idle);
    let idle_queen_count  = popcntf(pos.queens  & pos.idle);

    // Get a value based on material to indicate endgame progression (~1 is mg, ~0 is eg)
    // The typical output is <= 64 and minimum is zero (two kings and up to three pawns).
    // De-emphasises queen and pawn material (Large amounts thereof are typical of endgames.)
    // This value is used to interpolate between middlegame and endgame table values.
    let mg_eg = (
            (actv_knight_count + idle_knight_count + actv_bishop_count + idle_bishop_count) * 3.0 +
            (actv_rook_count + idle_rook_count + actv_queen_count + idle_queen_count) * 5.0 +
            (actv_pawn_count + idle_pawn_count) / 3.0
        ) / 64.0;

    // tally up material
    eval += (actv_pawn_count   - idle_pawn_count  ) * lerp(PIECE_MATERIAL[PAWN_IDX  ], mg_eg);
    eval += (actv_knight_count - idle_knight_count) * lerp(PIECE_MATERIAL[KNIGHT_IDX], mg_eg);
    eval += (actv_bishop_count - idle_bishop_count) * lerp(PIECE_MATERIAL[BISHOP_IDX], mg_eg);
    eval += (actv_rook_count   - idle_rook_count  ) * lerp(PIECE_MATERIAL[ROOK_IDX  ], mg_eg);
    eval += (actv_queen_count  - idle_queen_count ) * lerp(PIECE_MATERIAL[QUEEN_IDX ], mg_eg);
    
    // approximate some sense of openness vs closedness of the position (~1 is closed, ~0 is open)
    // max 32 (realistically, 15 is very closed, 7 is very open)
    const CENTRE: u64 = 0x183C7E7E3C1800;
    let op_cl = popcntf(pos.all & CENTRE) + popcntf(pos.pawns & CENTRE & pos.pawns << 0o10);
    let op_cl = ((op_cl as f32) - 5.0) / 10.0;

    // penalise knights in open positions and rooks in closed positions
    eval += (actv_knight_count - idle_knight_count) * lerp(KNIGHT_OPEN_POS, op_cl);
    eval += (actv_rook_count   - idle_rook_count  ) * lerp(ROOK_OPEN_POS,   op_cl);

    // penalise bishops whose pawns occupy the same squares most (offset to avoid overly devaluing bishops)
    eval += popcntf(pos.actv & pos.bishops & DARK ) * (popcntf(pos.actv & pos.pawns & DARK )-2.4) * lerp(BISHOP_PAWN_SQ, mg_eg);
    eval += popcntf(pos.actv & pos.bishops & LIGHT) * (popcntf(pos.actv & pos.pawns & LIGHT)-2.4) * lerp(BISHOP_PAWN_SQ, mg_eg);
    eval -= popcntf(pos.idle & pos.bishops & DARK ) * (popcntf(pos.idle & pos.pawns & DARK )-2.4) * lerp(BISHOP_PAWN_SQ, mg_eg);
    eval -= popcntf(pos.idle & pos.bishops & LIGHT) * (popcntf(pos.idle & pos.pawns & LIGHT)-2.4) * lerp(BISHOP_PAWN_SQ, mg_eg);

    // favour bishop pairs
    eval += (
        (actv_bishop_count >= 2.0) as i8 - (idle_bishop_count >= 2.0) as i8
    ) as f32 * lerp(BISHOP_PAIR, mg_eg);

    // favour rooks on open-er files
    for_sq!(sq in pos.rooks & pos.actv => {
        eval += popcntf(FILE << (sq%8) & pos.pawns) * lerp(ROOK_PAWN_FILE, mg_eg);
    });
    for_sq!(sq in pos.rooks & pos.idle => {
        eval -= popcntf(FILE << (sq%8) & pos.pawns) * lerp(ROOK_PAWN_FILE, mg_eg);
    });


    // major+minor piece-square tables
    // safe mobility
    let ma_mi_piece_data = [
        (pos.knights, KNIGHT_IDX, fend::knight_fend_wall    as fn(u8, u64) -> u64), 
        (pos.bishops, BISHOP_IDX, fend::bishop_fend         as fn(u8, u64) -> u64), 
        (pos.rooks,   ROOK_IDX,   fend::rook_fend           as fn(u8, u64) -> u64), 
        (pos.queens,  QUEEN_IDX,  fend::queen_fend          as fn(u8, u64) -> u64), 
    ];
    let mut actv_cvrd = fend::pawns_fend_actv(pos.pawns & pos.actv);
    let mut idle_cvrd = fend::pawns_fend_idle(pos.pawns & pos.idle);
    for (bb, idx, fend_fn) in ma_mi_piece_data {
        let mut temp_cvrd = 0;
        for_sq!(sq in bb & pos.actv => {
            let (rank, ffile) = (sq as usize/8, fold_file(sq as usize%8));
            eval += lerp(PIECE_SQ[idx][rank][ffile], mg_eg);
            let fend = fend_fn(sq, pos.all);
            temp_cvrd |= fend;
            eval += popcntf(fend & !idle_cvrd & !pos.actv) * MOBILITY;
        });
        for_sq!(sq in bb & pos.idle => {
            let (irank, ffile) = (7-sq as usize/8, fold_file(sq as usize%8));
            eval -= lerp(PIECE_SQ[idx][irank][ffile], mg_eg);
            let fend = fend_fn(sq, pos.all);
            idle_cvrd |= fend;
            eval -= popcntf(fend & !actv_cvrd & !pos.idle) * MOBILITY;
        });
        actv_cvrd |= temp_cvrd;
    }


    // fend space & piece attack+defence
    eval += (popcntf(actv_cvrd) - popcntf(idle_cvrd)) * COVERED;
    // rough heuristic; quescience searching should be doing most of this legwork
    eval += (popcntf(actv_cvrd & pos.all) - popcntf(idle_cvrd & pos.all)) * FEND;

    // king safety + piece sq
    let actv_king_sq = pos.actv_king_sq as usize;
    eval += lerp(PIECE_SQ[KING_IDX][  actv_king_sq/8][fold_file(actv_king_sq%8)], mg_eg);
    let actv_king_ring = fend::king_fend(pos.actv_king_sq)/*  &! (IDLE_PASSED_MASKS[actv_king_sq%8] >> 7-actv_king_sq/8) */;
    eval += popcntf(actv_king_ring & pos.pawns & pos.actv) * lerp(KING_RING_PAWN, mg_eg);
    eval += popcntf(idle_cvrd & actv_king_ring) * lerp(KING_RING_ATT, mg_eg);

    let idle_king_sq = pos.idle_king_sq as usize;
    eval -= lerp(PIECE_SQ[KING_IDX][7-idle_king_sq/8][fold_file(idle_king_sq%8)], mg_eg);
    let idle_king_ring = fend::king_fend(pos.idle_king_sq)/*  &! (ACTV_PASSED_MASKS[idle_king_sq%8] << idle_king_sq/8) */;
    eval -= popcntf(idle_king_ring & pos.pawns & pos.idle) * lerp(KING_RING_PAWN, mg_eg);
    eval -= popcntf(actv_cvrd & idle_king_ring) * lerp(KING_RING_ATT, mg_eg);

    // pawn structure
    let actv_pawns = pos.pawns & pos.actv;
    let idle_pawns = pos.pawns & pos.idle;
    for_sq!(sq in actv_pawns => {
        let (rank, file) = (sq / 8, (sq % 8) as usize);
        eval += rank.saturating_sub(2) as f32 * lerp(PAWN_RANK_SQRT, mg_eg).powf(3.0);
        eval += popcntf(SUPPORT_MASKS[file] << (rank-1)*8 & actv_pawns) * lerp(PAWN_SUPPORTED, mg_eg);
        if ACTV_PASSED_MASKS[file] << (rank-1)*8 & idle_pawns == 0 { eval += lerp(PAWN_PASSED, mg_eg); }
        if ISOLATED_MASKS[file] & actv_pawns == 0 { eval += lerp(PAWN_ISOLATED, mg_eg); }
        if FILE << file & actv_pawns & !(1 << sq) != 0 { eval += lerp(PAWN_DOUBLED, mg_eg); }
    });
    for_sq!(sq in idle_pawns => {
        let (rank, file) = (sq / 8, (sq % 8) as usize);
        eval -= 5u8.saturating_sub(rank) as f32 * lerp(PAWN_RANK_SQRT, mg_eg).powf(3.0);
        eval -= popcntf(SUPPORT_MASKS[file] << (rank-1)*8 & idle_pawns) * lerp(PAWN_SUPPORTED, mg_eg);
        if IDLE_PASSED_MASKS[file] >> (6-rank)*8 & actv_pawns == 0 { eval -= lerp(PAWN_PASSED, mg_eg); }
        if ISOLATED_MASKS[file] & idle_pawns == 0 { eval -= lerp(PAWN_ISOLATED, mg_eg); }
        if FILE << file & idle_pawns & !(1 << sq) != 0 { eval -= lerp(PAWN_DOUBLED, mg_eg); }
    });

    eval as i16
}

const FILE: u64 = 0x0101010101010101;
const LIGHT: u64 = 0x55AA55AA55AA55AA;
const DARK: u64 = 0xAA55AA55AA55AA55;

const KING_IDX: usize   = Piece::King as usize;
const QUEEN_IDX: usize  = Piece::Queen as usize;
const ROOK_IDX: usize   = Piece::Rook as usize;
const BISHOP_IDX: usize = Piece::Bishop as usize;
const KNIGHT_IDX: usize = Piece::Knight as usize;
const PAWN_IDX: usize   = Piece::Pawn as usize;

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
pub const PIECE_MATERIAL: [[f32; 2]; 6] = [
    [0.0, 0.0],
    [970.0, 920.0],
    [500.0, 570.0],
    [330.0, 335.0],
    [340.0, 290.0],
    [100.0, 120.0],
];


const MOBILITY: f32 = 5.0; // safe mobility bonus
const COVERED: f32 = 3.0; // tile fend bonus
const FEND: f32 = 4.0; // occupied tile fend extra bonus


// ~1: closed -> ~0: open
const KNIGHT_OPEN_POS: [f32; 2] =  [60.0, -15.0];
const ROOK_OPEN_POS  : [f32; 2] = [-50.0,  40.0];

// ~1: middle -> ~0: end
const BISHOP_PAIR   : [f32; 2] = [ 15.0,  30.0];
const BISHOP_PAWN_SQ: [f32; 2] = [ -3.0,  -8.0];

const ROOK_PAWN_FILE: [f32; 2] = [-20.0,  -4.0];

const PAWN_DOUBLED  : [f32; 2] = [-16.0, -25.0];
const PAWN_ISOLATED : [f32; 2] = [-12.0,  -5.0];
const PAWN_SUPPORTED: [f32; 2] = [ 12.0,  20.0];
const PAWN_PASSED   : [f32; 2] = [ 30.0,  65.0];
const PAWN_RANK_SQRT: [f32; 2] = [  2.0,   3.4];
const KING_RING_PAWN: [f32; 2] = [ 60.0,  12.0];
const KING_RING_ATT : [f32; 2] = [-50.0, -30.0];


// Stockfish piece-square table values, ripped from here on 2022/07/05:
// https://github.com/official-stockfish/Stockfish/blob/master/src/psqt.cpp


const PIECE_SQ: [[[[f32; 2]; 4]; 8]; 5] = [
    [ // King
        [ [271.0,  1.0], [327.0, 45.0], [271.0, 85.0], [198.0, 76.0] ],
        [ [278.0, 53.0], [303.0,100.0], [234.0,133.0], [179.0,135.0] ],
        [ [195.0, 88.0], [258.0,130.0], [169.0,169.0], [120.0,175.0] ],
        [ [164.0,103.0], [190.0,156.0], [138.0,172.0], [ 98.0,172.0] ],
        [ [154.0, 96.0], [179.0,166.0], [105.0,199.0], [ 70.0,199.0] ],
        [ [123.0, 92.0], [145.0,172.0], [ 81.0,184.0], [ 31.0,191.0] ],
        [ [ 88.0, 47.0], [120.0,121.0], [ 65.0,116.0], [ 33.0,131.0] ],
        [ [ 59.0, 11.0], [ 89.0, 59.0], [ 45.0, 73.0], [ -1.0, 78.0] ]
    ],
    [ // Queen
        [ [ 3.0,-69.0], [-5.0,-57.0], [-5.0,-47.0], [ 4.0,-26.0] ],
        [ [-3.0,-54.0], [ 5.0,-31.0], [ 8.0,-22.0], [12.0, -4.0] ],
        [ [-3.0,-39.0], [ 6.0,-18.0], [13.0, -9.0], [ 7.0,  3.0] ],
        [ [ 4.0,-23.0], [ 5.0, -3.0], [ 9.0, 13.0], [ 8.0, 24.0] ],
        [ [ 0.0,-29.0], [14.0, -6.0], [12.0,  9.0], [ 5.0, 21.0] ],
        [ [-4.0,-38.0], [10.0,-18.0], [ 6.0,-11.0], [ 8.0,  1.0] ],
        [ [-5.0,-50.0], [ 6.0,-27.0], [10.0,-24.0], [ 8.0, -8.0] ],
        [ [-2.0,-74.0], [-2.0,-52.0], [ 1.0,-43.0], [-2.0,-34.0] ]
    ],
    [ // Rook
        [ [-31.0, -9.0], [-20.0,-13.0], [-14.0,-10.0], [-5.0, -9.0] ],
        [ [-21.0,-12.0], [-13.0, -9.0], [ -8.0, -1.0], [ 6.0, -2.0] ],
        [ [-25.0,  6.0], [-11.0, -8.0], [ -1.0, -2.0], [ 3.0, -6.0] ],
        [ [-13.0, -6.0], [ -5.0,  1.0], [ -4.0, -9.0], [-6.0,  7.0] ],
        [ [-27.0, -5.0], [-15.0,  8.0], [ -4.0,  7.0], [ 3.0, -6.0] ],
        [ [-22.0,  6.0], [ -2.0,  1.0], [  6.0, -7.0], [12.0, 10.0] ],
        [ [ -2.0,  4.0], [ 12.0,  5.0], [ 16.0, 20.0], [18.0, -5.0] ],
        [ [-17.0, 18.0], [-19.0,  0.0], [ -1.0, 19.0], [ 9.0, 13.0] ]
    ],
    [ // Bishop
        [ [-37.0,-40.0], [ -4.0,-21.0], [ -6.0,-26.0], [-16.0, -8.0] ],
        [ [-11.0,-26.0], [  6.0, -9.0], [ 13.0,-12.0], [  3.0,  1.0] ],
        [ [ -5.0,-11.0], [ 15.0, -1.0], [ -4.0, -1.0], [ 12.0,  7.0] ],
        [ [ -4.0,-14.0], [  8.0, -4.0], [ 18.0,  0.0], [ 27.0, 12.0] ],
        [ [ -8.0,-12.0], [ 20.0, -1.0], [ 15.0,-10.0], [ 22.0, 11.0] ],
        [ [-11.0,-21.0], [  4.0,  4.0], [  1.0,  3.0], [  8.0,  4.0] ],
        [ [-12.0,-22.0], [-10.0,-14.0], [  4.0, -1.0], [  0.0,  1.0] ],
        [ [-34.0,-32.0], [  1.0,-29.0], [-10.0,-26.0], [-16.0,-17.0] ]
    ],
    [ // Knight
        [ [-175.0, -96.0], [-92.0,-65.0], [-74.0,-49.0], [-73.0,-21.0] ],
        [ [ -77.0, -67.0], [-41.0,-54.0], [-27.0,-18.0], [-15.0,  8.0] ],
        [ [ -61.0, -40.0], [-17.0,-27.0], [  6.0, -8.0], [ 12.0, 29.0] ],
        [ [ -35.0, -35.0], [  8.0, -2.0], [ 40.0, 13.0], [ 49.0, 28.0] ],
        [ [ -34.0, -45.0], [ 13.0,-16.0], [ 44.0,  9.0], [ 51.0, 39.0] ],
        [ [  -9.0, -51.0], [ 22.0,-44.0], [ 58.0,-16.0], [ 53.0, 17.0] ],
        [ [ -67.0, -69.0], [-27.0,-50.0], [  4.0,-51.0], [ 37.0, 12.0] ],
        [ [-201.0,-100.0], [-83.0,-88.0], [-56.0,-56.0], [-26.0,-17.0] ]
    ],
];

