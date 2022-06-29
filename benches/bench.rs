
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use infra::board::{Board, Piece, mov::MoveSetTable, Move};

fn bench_board_methods(c: &mut Criterion) {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let board = infra::board::Board::from_fen(fen).unwrap();
    c.bench_function("make", |b| b.iter(|| {
        let mut board = board;
        board.make(Move::new(0o44, 0o25, Piece::Knight));
    }));
    c.bench_function("get_moveset_actv", |b| b.iter(|| {
        let mut board = board;
        let mut sets = MoveSetTable::new();
        board.get_moveset_actv(&mut sets);
    }));
    c.bench_function("get_moveset_actv + make all", |b| b.iter(|| {
        let mut board = board;
        let mut sets = MoveSetTable::new();
        board.get_moveset_actv(&mut sets);
        for ss in sets.get_move_sets() {
            for mov in ss.iter() {
                if board.is_move_legal(mov) {
                    let mut board = board;
                    board.make(mov);
                    let x = black_box(&mut board);
                } else {
                    panic!();
                }
            }
        }
        for ss in sets.get_prom_sets() {
            for prom in ss.iter() {
                if board.is_move_legal(prom) {
                    let mut board = board;
                    board.make(prom);
                    let x = black_box(&mut board);
                } else {
                    panic!();
                }
            }
        }
    }));
    c.bench_function("is_game_over", |b| b.iter(|| {
        let x = board.is_game_over();
        //assert!(x.is_none());
    }));
    c.bench_function("is_actv_in_check", |b| b.iter(|| {
        let x = board.is_actv_in_check();
        //assert!(!x);
    }));
    c.bench_function("calc_actv_fend", |b| b.iter(|| {
        let x = board.calc_actv_fend();
        //assert!(!x);
    }));
}


criterion_group!(benches, bench_board_methods);
criterion_main!(benches);
