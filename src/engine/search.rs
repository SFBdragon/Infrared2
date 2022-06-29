use crate::Board;



/* pub fn negamax(board: &Board, depth: usize, mut a: isize, b: isize) -> isize {
    if depth == 0 { return super::eval::material_eval(board); }

    let mut max = isize::MIN;

    let mut move_table = crate::board::mov::MoveSetTable::new();
    board.get_moveset_actv(&mut move_table);
    for ss in move_table.get_prom_sets() {
        for prom in ss.iter() {
            let mut board = board.clone();
            if board.make(prom) {
                
                let score = -negamax(&board, depth - 1, -b, -a);
                if score > max { max = score; }
                if score > a { a = score; }

            }
        }
    }
    for ss in move_table.get_move_sets() {
        for mov in ss.iter() {
            let mut board = board.clone();
            if board.make(mov) {

                /* score = -negaMax( depth - 1 );
                if( score > max )
                    max = score; */

            }
        }
    }
    
    return max;
} */