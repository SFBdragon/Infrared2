mod uci;

fn main() {
    // handle/config args
    for _arg in std::env::args() { }

    // test()
    
    // Run UCI Engine!
    uci::uci();
}


#[allow(unused)]
fn test() {
    let fen = "r1bqkb1r/pppnppp1/5n1p/3p2B1/3P4/3Q4/PPP1PPPP/RN2KBNR w KQkq - 0 1";

    let game = infra::Game::new(infra::Board::from_fen(fen).unwrap(), Vec::new()).unwrap();
    let (sndr, rcvr) = crossbeam_channel::unbounded();
    
    let t1 = std::time::Instant::now();
    let _ = game.search(infra::TimeControl::Infinite, None, sndr);
    
    while let Ok((info, is_final)) = rcvr.recv() {
        let t2 = std::time::Instant::now();
        println!("{:?} : {} : {:?}", (t2 - t1).as_secs_f32(), info.depth, &info.evals[0..2]);
        if is_final { break; }
    }
}
