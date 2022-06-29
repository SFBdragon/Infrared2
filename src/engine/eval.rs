use crate::Board;

pub fn material_eval(board: &Board) -> isize {
    let mut material = 0;

    material += ((board.pawns & board.actv).count_ones() as isize - (board.pawns & board.idle).count_ones() as isize) * 110;
    material += ((board.knights & board.actv).count_ones() as isize - (board.knights & board.idle).count_ones() as isize) * 300;
    material += ((board.bishops & board.actv).count_ones() as isize - (board.bishops & board.idle).count_ones() as isize) * 310;
    material += ((board.rooks & board.actv).count_ones() as isize - (board.rooks & board.idle).count_ones() as isize) * 510;
    material += ((board.queens & board.actv).count_ones() as isize - (board.queens & board.idle).count_ones() as isize) * 880;

    material
}