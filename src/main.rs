mod uci;

fn main() {
    // handle/config args
    for _arg in std::env::args() { }

    let fen = "1r3rk1/pppq2p1/1n1pb2p/3N4/2PQ4/6P1/PP3PBP/4RRK1 w - - 0 1";
    let game = infra::Game::new(infra::Board::from_fen(fen).unwrap(), Vec::new()).unwrap();
    
    let t1 = std::time::Instant::now();
    let handle = game.search(infra::TimeControl::Infinite, None);
    
    while let Ok((info, is_final)) = handle.info_receiver.recv() {
        let t2 = std::time::Instant::now();
        println!("{:?} : {:?}", (t2 - t1).as_secs_f32(), info);
    }
    
    // Run UCI Engine!
    uci::uci();
}



/* pub fn nm(board: &Board, draft: u8, mut alpha: i16, beta: i16) -> i16 {
    if draft == 0 { return eval(board); }

    let mut move_table = crate::board::mov::MoveSetTable::new();
    board.get_move_tab_actv(&mut move_table);
    
    let mut max = -i16::MAX;
    for_mov!(mov in ai move_table until 'brk => {
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
} */
