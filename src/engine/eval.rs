use crate::Board;

pub fn material_eval(board: &Board) -> isize {
    let mut material = 0;

    material += (board.actv_pawns.count_ones() as isize -   board.idle_pawns.count_ones() as isize) * 110;
    material += (board.actv_knights.count_ones() as isize - board.idle_knights.count_ones() as isize) * 300;
    material += (board.actv_bishops.count_ones() as isize - board.idle_bishops.count_ones() as isize) * 310;
    material += (board.actv_rooks.count_ones() as isize -   board.idle_rooks.count_ones() as isize) * 510;
    material += (board.actv_queens.count_ones() as isize -  board.idle_queens.count_ones() as isize) * 880;

    material
}
