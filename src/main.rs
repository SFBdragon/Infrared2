
mod gen_magics;

fn main() {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let mut board = infra::board::Board::from_fen(fen).unwrap();
    board.make(infra::Move::new(4, 6, infra::Piece::King));
    dbg!(board.to_fen());
    let mut moves = infra::board::mov::MoveSetTable::new();
    board.get_moveset_actv(&mut moves);
    dbg!(moves.get_move_sets());
    dbg!(moves.get_prom_sets());
}
