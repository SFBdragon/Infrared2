mod uci;

fn main() {
    // handle/config args
    for _arg in std::env::args() { }

    let fen = "1r3rk1/pppq2p1/1n1pb2p/3N4/2PQ4/6P1/PP3PBP/4RRK1 w - - 0 1";
    let board = infra::Board::from_fen(fen).unwrap();
    let ttab = std::sync::Arc::new(infra::search::ttab::TransTable::default());
    let ks = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let phm = std::sync::Arc::new(infra::PosHashMap::with_hasher(infra::board::zobrist::U64IdentHashBuilder));

    let (sndr, rcvr) = crossbeam_channel::unbounded();
    
    let t1 = std::time::Instant::now();
    std::thread::spawn(move || {
        infra::search::search(board.clone(), sndr, ttab.clone(), phm.clone(), ks.clone());
    });

    while let Ok(info) = rcvr.recv() {
        let t2 = std::time::Instant::now();
        println!("{:?}", info);
        println!("{:?}", (t2 - t1).as_secs_f32());
    }
    
    // Run UCI Engine!
    uci::uci();
}
