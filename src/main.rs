mod uci;

fn main() {
    // handle/config args
    for _arg in std::env::args() { }
    
    
    let mate_fen = "8/8/1k6/8/8/2R5/8/6RK w - - 0 1";
    let fen = "r1bqkb1r/pppp2pp/2n3n1/1B2p1N1/4P3/8/PPPP1PPP/RNBQK2R w KQkq - 0 4";
    let mut board = infra::Board::from_fen(fen).unwrap();
    let ttab = infra::engine::ttab::TransTable::with_memory(1024 * 1024 * 1024 * 2);
    let ks = std::sync::atomic::AtomicBool::new(false);
    let phm = infra::PosHashMap::with_hasher(infra::board::zobrist::U64IdentHashBuilder);
    let mut search_data = infra::engine::SearchData {
        kill_switch: &ks,
        trans_table: &ttab,
        pos_hash_map: &phm,

        node_count: 0,
        check_exts: 0,
    };
    
    /* let t1 = std::time::Instant::now();
    let score = nm(&board, 7, f32::NEG_INFINITY, f32::INFINITY);
    let t2 = std::time::Instant::now();
    println!("{}", (t2 - t1).as_secs_f32());
    dbg!(score); */
    
    let t1 = std::time::Instant::now();
    let score1 = infra::engine::negamax(&mut board, -i16::MAX, i16::MAX, 6, &mut search_data, None);
    board.make(infra::Move::new(0o15, 0o25, infra::Piece::Pawn));
    let score2 = infra::engine::negamax(&mut board, -i16::MAX, i16::MAX, 6, &mut search_data, None);
    let t2 = std::time::Instant::now();
    dbg!(score1);
    dbg!(score2);
    println!("total: {}s", (t2 - t1).as_secs_f32());

    
    /* let t1 = std::time::Instant::now();
    let score = nm(&board, 7, f32::NEG_INFINITY, f32::INFINITY);
    let t2 = std::time::Instant::now();
    println!("{}", (t2 - t1).as_secs_f32());
    dbg!(score); */

    return;
    
    // Run UCI Engine!
    uci::uci();
}

fn nm(board: &infra::Board, draft: u8, mut alpha: i16, beta: i16) -> i16 {
    if draft == 0 { return infra::engine::eval::eval(board); }

    let mut move_table = infra::board::mov::MoveSetTable::new();
    board.get_move_tab_actv(&mut move_table);
    
    let mut max = -i16::MAX;
    infra::for_mov!(mov in ai move_table until 'brk => {
        if board.is_move_legal(mov) {
            let mut board = board.clone();
            board.make(mov);
            let score = -nm(&board, draft - 1, -beta, -alpha);
            if score > max { max = score; }
            if max > alpha { alpha = max; }
            if alpha >= beta { break 'brk; }
        }
    });
    return max
}

