mod gen_magics;

fn main() {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let mut board = infra::board::Board::from_fen(fen).unwrap();
    let comparison = board.clone();
    dbg!(&board);
    let mut sets = infra::board::mov::MoveSetTable::new();
    board.get_moveset_actv(&mut sets);
    for ss in sets.get_move_sets() {
        for mov in ss.iter() {
            if board.is_move_legal(mov) {
                let unmake = board.make(mov);
                board.unmake(mov, &unmake);
            }
        }
    }

    dbg!(&board);
    assert_eq!(board, comparison);
}
