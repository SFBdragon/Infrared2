#![feature(maybe_uninit_slice)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use infra::board::{Board, Piece, mov::MoveSetTable};

fn bench_board_methods(c: &mut Criterion) {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let board = infra::board::Board::from_fen(fen).unwrap();
    c.bench_function("get_moveset_actv + make all", |b| b.iter(|| {
        let mut board = board;
        let mut sets = MoveSetTable::new();
        board.get_moveset_actv(&mut sets);
        for ss in sets.get_move_sets() {
            for (from, to, piece) in ss.iter() {
                let mut board = board;
                let x = board.make(from, to, piece);
            }
        }
        for ss in sets.get_prom_sets() {
            for (from, to, piece) in ss.iter() {
                let mut board = board;
                let x = board.make(from, to, piece);
            }
        }
    }));
    c.bench_function("init_fends", |b| b.iter(|| {
        let mut board = board;
        board.calc_actv_fend();
        board.calc_idle_fend();
        let x = black_box(board.actv_fend);
        let y = black_box(board.idle_fend);
    }));
    c.bench_function("is_game_over", |b| b.iter(|| {
        let x = board.is_game_over();
        assert!(x.is_none());
    }));
}


criterion_group!(benches, bench_board_methods);
criterion_main!(benches);
