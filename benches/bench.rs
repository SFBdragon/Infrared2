
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use infra::{Board, Piece, Move, Sq, search::ord::MoveBuffer};

fn bench_board_methods(c: &mut Criterion) {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let board = Board::from_fen(fen).unwrap();


    c.bench_function("make_hash", |b| b.iter(|| {
        board.make_hash(Move::new(Sq::F3, Sq::E5, Piece::Knight))
    }));

    c.bench_function("for_move", |b| b.iter(|| {
        board.for_move(|mov| {
            let mut b = board.clone();
            b.make(mov);
            black_box(false)
        });
    }));
    c.bench_function("mvbf/gen", |b| b.iter(|| {
        let mut mvbf = MoveBuffer::new();
        board.gen_all(&mut mvbf);

        for &mut mv in mvbf.as_mut_slice() {
            if board.is_legal(mv) {
                let mut b = board.clone();
                b.make(mv);
            }
        }
    }));
    c.bench_function("is_valid", |b| b.iter(|| {
        let mov = Move::new(Sq::F3, Sq::E5, Piece::Knight);
        black_box(board.is_valid(black_box(mov)));
    }));
    c.bench_function("make", |b| b.iter(|| {
        let mov = Move::new(Sq::F3, Sq::E5, Piece::Knight);
        let mut b = board.clone();
        /* let unmake =  */b.make(mov);
        /* board.unmake(mov, &unmake); */
    }));
    let board = Board::from_fen(fen).unwrap();
    c.bench_function("is_game_over", |b| b.iter(|| {
        let _ = board.is_game_over();
        //assert!(x.is_none());
    }));
    c.bench_function("is_actv_in_check", |b| b.iter(|| {
        let x = board.in_check();
        assert!(!x);
    }));
}


criterion_group!(benches, bench_board_methods);
criterion_main!(benches);
