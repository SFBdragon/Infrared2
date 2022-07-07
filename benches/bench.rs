
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use infra::{board::{Board, Piece, mov::MoveSetTable, Move}, engine::{negamax, ttab}};

fn bench_board_methods(c: &mut Criterion) {
    let fen = "r1bqkbnr/pppp2pp/2n5/1B2pp2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let mut board = infra::board::Board::from_fen(fen).unwrap();
    /* c.bench_function("get_moveset_actv + make all", |b| b.iter(|| {
        let mut sets = MoveSetTable::new();
        board.get_move_tab_actv(&mut sets);
        infra::for_mov!(mov in sets => {
            if board.is_move_legal(mov) {
                let mut board = board.clone();
                board.make(mov);
            } else {
                panic!();
            }
        });
    })); */
    //let mut ks = std::sync::atomic::AtomicBool::new(false);
    /* c.bench_function("negamax", |b| b. b.iter(|| {
        let ttab = ttab::TransTable::with_memory(1024 * 1024/*  * 1024 * 2 */);
        let score = -negamax(&board, -i32::MAX, i32::MAX, /* 0, */ 6, &ttab, &ks);
        black_box(score);
    })); */
    
    println!("eval: {}", infra::engine::eval::eval(&Board::default()));
    println!("eval: {}", infra::engine::eval::eval(&board));
    println!("eval: {}", infra::engine::eval::eval(&Board::from_fen("2k5/8/3P1K2/2B3P1/2b5/8/8/8 b - - 0 1").unwrap()));
    println!("eval: {}", infra::engine::eval::eval(&Board::from_fen("r1bq1rk1/pp3ppp/2n1pn2/3p4/1bPP4/3B1N2/PP1BNPPP/R2QK2R w KQ - 2 5").unwrap()));
    return;
    c.bench_function("eval", |b| b.iter(|| {
        black_box(infra::engine::eval::eval(black_box(&board)));
    }));
    c.bench_function("make", |b| b.iter(|| {
        let mov = Move::new(0o25, 0o44, Piece::Knight);
        let mut board = board.clone();
        /* let unmake =  */board.make(mov);
        /* board.unmake(mov, &unmake); */
    }));
    c.bench_function("get_moveset_actv", |b| b.iter(|| {
        let mut sets = MoveSetTable::new();
        board.get_move_tab_actv(&mut sets);
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
    /* c.bench_function("calc_actv_fend", |b| b.iter(|| {
        let x = board.calc_actv_fend();
        //assert!(!x);
    })); */
}


criterion_group!(benches, bench_board_methods);
criterion_main!(benches);
