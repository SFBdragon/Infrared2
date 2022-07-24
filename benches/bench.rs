
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use infra::{board::{Board, Piece, Move}, search::{pvs, ttab}};

fn bench_board_methods(c: &mut Criterion) {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let mut board = infra::Board::from_fen(fen).unwrap();
    c.bench_function("for_mov", |b| b.iter(|| {
        board.for_mov(|mov| {
            let mut b = board.clone();
            b.make(mov);
            false
        });
    }));
    c.bench_function("is_valid", |b| b.iter(|| {
        let mov = Move::new(0o25, 0o44, Piece::Knight);
        black_box(board.is_valid(black_box(mov)));
    }));
    c.bench_function("make", |b| b.iter(|| {
        let mov = Move::new(0o25, 0o44, Piece::Knight);
        let mut b = board.clone();
        /* let unmake =  */b.make(mov);
        /* board.unmake(mov, &unmake); */
    }));
    let mut board = infra::board::Board::from_fen(fen).unwrap();
    c.bench_function("is_game_over", |b| b.iter(|| {
        let x = board.is_game_over();
        //assert!(x.is_none());
    }));
    c.bench_function("is_actv_in_check", |b| b.iter(|| {
        let x = board.is_actv_in_check();
        assert!(!x);
    }));
}


criterion_group!(benches, bench_board_methods);
criterion_main!(benches);
