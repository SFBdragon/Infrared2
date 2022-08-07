//! Chess search engine tests for benchmarking and iterating.

use std::time::Duration;
use crossbeam_channel::Receiver;

use infra::{TimeControl, Game, SearchInfo, Board};
use infra::epd::{self, Epd};


pub fn test_pos() {
    let fen = "r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - - 0 1";

    let game = Game::new(Board::from_fen(fen).unwrap(), vec![]).unwrap();
    let handle = game.search(TimeControl::Infinite, None);
    let _ = log_search_info(handle.info_channel.1, &game.position);
}

pub fn log_search_info(rcvr: Receiver<(SearchInfo, bool)>, pos: &Board) -> SearchInfo {
    let t1 = std::time::Instant::now();
    while let Ok((info, is_final)) = rcvr.recv() {
        let t2 = std::time::Instant::now();
        print!("\nTime: {:12} | Depth: {:2} | ", (t2 - t1).as_secs_f32(), info.depth);
        for &(mov, eval) in info.evals.iter().take(3) {
            print!("{} {}, ", mov.to_lan(pos), match eval {
                infra::SearchEval::Normal(score) => (score as f32 / 100.0).to_string(),
                infra::SearchEval::Mate(dist) => "M".to_owned() + dist.to_string().as_str(),
            });
        }
        if is_final { return info; }
    }
    unreachable!();
}

pub fn epd_bm_am_assessment(name: &str, epd: &Epd, info: &SearchInfo) -> Option<bool> {
    print!("\n{}", name);
    if let Some(id) = epd.id.as_ref() { print!(" | ID: {}", id.as_str()) }
    let fm = info.evals[0].0;
    print!(" | Engine move: {}", fm.to_lan(&epd.pos));
    let mut status = None;
    if let Some(bm) = epd.bm.as_ref() {
        let is_bm = bm.contains(&fm);
        status = status.map_or(Some(is_bm), |s| Some(s && is_bm));
        print!(" | Best moves: {:?} [{}]", 
            bm.iter().map(|m| m.to_lan(&epd.pos)).collect::<Vec<_>>(),
            if is_bm { "FOUND" } else { "NOT FOUND" }
        );
    }
    if let Some(am) = epd.am.as_ref() {
        let isnt_am = !am.contains(&fm);
        status = status.map_or(Some(isnt_am), |s| Some(s && isnt_am));
        print!(" | Avoid moves: {:?} [{}]", 
            am.iter().map(|m| m.to_lan(&epd.pos)).collect::<Vec<_>>(), 
            if isnt_am { "AVOIDED" } else { "NOT AVOIDED" }
        );
    }
    println!();
    status
}

fn epd_test(name: &str, epds: &str, time_control: TimeControl) {
    println!("{} Test", name);

    let mut total = 0;
    let mut score = 0;

    for epd in epd::parse_epd(epds.trim()) {
        let game = Game::new(epd.pos.clone(), vec![]).unwrap();
        let handle = game.search(time_control, None);
        let info = log_search_info(handle.info_channel.1, &epd.pos);
        let status = epd_bm_am_assessment(name, &epd, &info);

        if let Some(status) = status {
            if status { score += 1; }
            total += 1;
        }
    }
    println!("{} test: {} / {}", name, score, total);
}

pub fn lct_ii_test() {
    epd_test("LCT II", LCTII, TimeControl::MoveTime(Duration::from_secs(30)));
}

pub fn brantko_kopec_test() {
    epd_test("Brantko-Kopec", BRANTKO_KOPEC, TimeControl::MoveTime(Duration::from_secs(30)));
}

pub fn eigmann_rapid_test() {
    epd_test("Eigmann Rapid", EIGMANN_RAPID, TimeControl::MoveTime(Duration::from_secs(15)));
}

pub fn silent_but_deadly_test() {
    epd_test("Silent but Deadly", SILENT_BUT_DEADLY, TimeControl::MoveTime(Duration::from_secs(30)));
}




/// LCT II, the Louguet Chess Test II was created by Frédéric Louguet in 1994 and 
/// published in the French computer chess magazine La Puce Echiquéenne n° 8. 
/// It contains 14 positional, 12 tactical and 9 endgame test-postions, 10 minutes per position. 
pub static LCTII: &str = r##"
r3kb1r/3n1pp1/p6p/2pPp2q/Pp2N3/3B2PP/1PQ2P2/R3K2R w KQkq - bm d6; id "LCTII.POS.01"; c0 "Chernin - Miles, Tunis 1985";
1k1r3r/pp2qpp1/3b1n1p/3pNQ2/2pP1P2/2N1P3/PP4PP/1K1RR3 b - - bm Bb4; id "LCTII.POS.02"; c0 "Lilienthal - Botvinnik, Moskau 1945";
r6k/pp4p1/2p1b3/3pP3/7q/P2B3r/1PP2Q1P/2K1R1R1 w - - bm Qc5; id "LCTII.POS.03"; c0 "Boissel - Boulard, corr. 1994";
1nr5/2rbkppp/p3p3/Np6/2PRPP2/8/PKP1B1PP/3R4 b - - bm e5; id "LCTII.POS.04"; c0 "Kaplan - Kopec, USA 1975";
2r2rk1/1p1bq3/p3p2p/3pPpp1/1P1Q4/P7/2P2PPP/2R1RBK1 b - - bm Bb5; id "LCTII.POS.05"; c0 "Estrin - Pytel, Albena 1973";
3r1bk1/p4ppp/Qp2p3/8/1P1B4/Pq2P1P1/2r2P1P/R3R1K1 b - - bm e5; id "LCTII.POS.06"; c0 "Nimzowitsch - Capablanca, New York 1927";
r1b2r1k/pp2q1pp/2p2p2/2p1n2N/4P3/1PNP2QP/1PP2RP1/5RK1 w - - bm Nd1; id "LCTII.POS.07"; c0 "Tartakower - Rubinstein, Moskau 1925";
r2qrnk1/pp3ppb/3b1n1p/1Pp1p3/2P1P2N/P5P1/1B1NQPBP/R4RK1 w - - bm Bh3; id "LCTII.POS.08"; c0 "Polugaevsky - Unzicker, Kislovodsk 1972";
5nk1/Q4bpp/5p2/8/P1n1PN2/q4P2/6PP/1R4K1 w - - bm Qd4; id "LCTII.POS.09"; c0 "Boissel - Del Gobbo, corr. 1994";
r3k2r/3bbp1p/p1nppp2/5P2/1p1NP3/5NP1/PPPK3P/3R1B1R b kq - bm Bf8; id "LCTII.POS.10"; c0 "Cucka - Jansa, Brno 1960";
bn6/1q4n1/1p1p1kp1/2pPp1pp/1PP1P1P1/3N1P1P/4B1K1/2Q2N2 w - - bm h4; id "LCTII.POS.11"; c0 "Landau - Schmidt, Noordwijk 1938";
3r2k1/pp2npp1/2rqp2p/8/3PQ3/1BR3P1/PP3P1P/3R2K1 b - - bm Rb6; id "LCTII.POS.12"; c0 "Korchnoi - Karpov, Meran 1981";
1r2r1k1/4ppbp/B5p1/3P4/pp1qPB2/2n2Q1P/P4PP1/4RRK1 b - - bm Nxa2; id "LCTII.POS.13"; c0 "Barbero - Kouatly, Budapest 1987";
r2qkb1r/1b3ppp/p3pn2/1p6/1n1P4/1BN2N2/PP2QPPP/R1BR2K1 w kq - bm d5; id "LCTII.POS.14"; c0 "Spasski - Aftonomov, Leningrad 1949";
1r4k1/1q2bp2/3p2p1/2pP4/p1N4R/2P2QP1/1P3PK1/8 w - - bm Nxd6; id "LCTII.CMB.01"; c0 "Romanishin - Gdansky, Polonica Zdroj 1992";
rn3rk1/pbppq1pp/1p2pb2/4N2Q/3PN3/3B4/PPP2PPP/R3K2R w KQ - bm Qxh7+; id "LCTII.CMB.02"; c0 "Lasker,Ed - Thomas, London 1911";
4r1k1/3b1p2/5qp1/1BPpn2p/7n/r3P1N1/2Q1RPPP/1R3NK1 b - - bm Qf3; id "LCTII.CMB.03"; c0 "Andruet - Spassky, BL 1988";
2k2b1r/1pq3p1/2p1pp2/p1n1PnNp/2P2B2/2N4P/PP2QPP1/3R2K1 w - - bm exf6; id "LCTII.CMB.04"; c0 "Vanka - Jansa, Prag 1957";
2r2r2/3qbpkp/p3n1p1/2ppP3/6Q1/1P1B3R/PBP3PP/5R1K w - - bm Rxh7+; id "LCTII.CMB.05"; c0 "Boros - Szabo, Budapest 1937";
2r1k2r/2pn1pp1/1p3n1p/p3PP2/4q2B/P1P5/2Q1N1PP/R4RK1 w q - bm exf6; id "LCTII.CMB.06"; c0 "Lilienthal - Capablanca, Hastings 1934";
2rr2k1/1b3ppp/pb2p3/1p2P3/1P2BPnq/P1N3P1/1B2Q2P/R4R1K b - - bm Rxc3; id "LCTII.CMB.07"; c0 "Rotlewi - Rubinstein, Lodz 1907";
2b1r1k1/r4ppp/p7/2pNP3/4Q3/q6P/2P2PP1/3RR1K1 w - - bm Nf6+; id "LCTII.CMB.08"; c0 "Zarkov - Mephisto, Albuquerque 1991";
6k1/5p2/3P2p1/7n/3QPP2/7q/r2N3P/6RK b - - bm Rxd2; id "LCTII.CMB.09"; c0 "Portisch - Kasparov, Moskau 1981";
rq2rbk1/6p1/p2p2Pp/1p1Rn3/4PB2/6Q1/PPP1B3/2K3R1 w - - bm Bxh6; id "LCTII.CMB.10"; c0 "Tchoudinovskikh - Merchiev, UdSSR 1987";
rnbq2k1/p1r2p1p/1p1p1Pp1/1BpPn1N1/P7/2P5/6PP/R1B1QRK1 w - - bm Nxh7; id "LCTII.CMB.11"; c0 "Vaisser - Genius 2, Aubervilliers, 1994";
r2qrb1k/1p1b2p1/p2ppn1p/8/3NP3/1BN5/PPP3QP/1K3RR1 w - - bm e5; id "LCTII.CMB.12"; c0 "Spassky - Petrosian, Moskau 1969";
8/1p3pp1/7p/5P1P/2k3P1/8/2K2P2/8 w - - bm f6; id "LCTII.FIN.01"; c0 "NN - Lasker,Ed";
8/pp2r1k1/2p1p3/3pP2p/1P1P1P1P/P5KR/8/8 w - - bm f5; id "LCTII.FIN.02"; c0 "Capablanca - Eliskases, Moskau 1936";
8/3p4/p1bk3p/Pp6/1Kp1PpPp/2P2P1P/2P5/5B2 b - - bm Bxe4; id "LCTII.FIN.03"; c0 "Studie 1994";
5k2/7R/4P2p/5K2/p1r2P1p/8/8/8 b - - bm h3; am h5; id "LCTII.FIN.04"; c0 "Karpov - Deep Thought, Analyse 1990";
6k1/6p1/7p/P1N5/1r3p2/7P/1b3PP1/3bR1K1 w - - bm a6; id "LCTII.FIN.05"; c0 "Karpov - Kasparov, Moskau 1985 [Analyse]";
8/3b4/5k2/2pPnp2/1pP4N/pP1B2P1/P3K3/8 b - - bm f4; id "LCTII.FIN.06"; c0 "Minev - Portisch, Halle 1967";
6k1/4pp1p/3p2p1/P1pPb3/R7/1r2P1PP/3B1P2/6K1 w - - bm Bb4; id "LCTII.FIN.07"; c0 "Lengyel - Kaufman, Los Angeles 1974";
2k5/p7/Pp1p1b2/1P1P1p2/2P2P1p/3K3P/5B2/8 w - - bm c5; id "LCTII.FIN.08"; c0 "Spassky - Byrne, 1974";
8/5Bp1/4P3/6pP/1b1k1P2/5K2/8/8 w - - bm Kg4; id "LCTII.FIN.09"; c0 "Klimenok - Kabanov, UdSSR 1969";"##;


/// The Bratko-Kopec Test was designed by Dr. Ivan Bratko and Dr. Danny Kopec in 1982 to 
/// evaluate human or machine chess ability based on the presence or absence of certain 
/// knowledge (i.e. Master, Expert, Novice, etc). This test has been a standard for nearly 
/// 20 years in computer chess. Experience has shown it very reliable in corresponding to 
/// the chess rating of humans and machines.
pub static BRANTKO_KOPEC: &str = r##"
1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 b - - bm Qd1+; id "BK.01";
3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - - bm d5; id "BK.02";
2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - - bm f5; id "BK.03";
rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq - bm e6; id "BK.04";
r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - - bm Nd5 a4; id "BK.05";
2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - - bm g6; id "BK.06";
1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - - bm Nf6; id "BK.07";
4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - - bm f5; id "BK.08";
2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - - bm f5; id "BK.09";
3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - - bm Ne5; id "BK.10";
2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - - bm f4; id "BK.11";
r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - - bm Bf5; id "BK.12";
r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - - bm b4; id "BK.13";
rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - - bm Qd2 Qe1; id "BK.14";
2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - - bm Qxg7+; id "BK.15";
r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq - bm Ne4; id "BK.16";
r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - - bm h5; id "BK.17";
r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - - bm Nb3; id "BK.18";
3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - - bm Rxe4; id "BK.19";
r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - - bm g4; id "BK.20";
3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - - bm Nh6; id "BK.21";
2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - - bm Bxe4; id "BK.22";
r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq - bm f6; id "BK.23";
r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - - bm f4; id "BK.24";"##;


/// The Eigenmann Rapid Engine Test, designed by Walter Eigenmann, 
/// is a collection of 111 test-positions covering a wide range of chess 
/// motives with unique solutions. The number of correct solutions in 15 seconds 
/// per position is the result to roughly compare engine skills.
pub static EIGMANN_RAPID: &str = r##"
r1bqk1r1/1p1p1n2/p1n2pN1/2p1b2Q/2P1Pp2/1PN5/PB4PP/R4RK1 w q - bm Rxf4; id "ERET 001 - Relief";
r1n2N1k/2n2K1p/3pp3/5Pp1/b5R1/8/1PPP4/8 w - - bm Ng6; id "ERET 002 - Zugzwang";
r1b1r1k1/1pqn1pbp/p2pp1p1/P7/1n1NPP1Q/2NBBR2/1PP3PP/R6K w - - bm f5; id "ERET 003 - Open Line";
5b2/p2k1p2/P3pP1p/n2pP1p1/1p1P2P1/1P1KBN2/7P/8 w - - bm Nxg5; id "ERET 004 - Endgame";
r3kbnr/1b3ppp/pqn5/1pp1P3/3p4/1BN2N2/PP2QPPP/R1BR2K1 w kq - - bm Bxf7; id "ERET 005 - Bishop Sacrifice f7";
r2r2k1/1p1n1pp1/4pnp1/8/PpBRqP2/1Q2B1P1/1P5P/R5K1 b - - bm Nc5; id "ERET 006 - Knight Sacrifice";
2rq1rk1/pb1n1ppN/4p3/1pb5/3P1Pn1/P1N5/1PQ1B1PP/R1B2RK1 b - - bm Nde5; id "ERET 007 - Bishop Pair";
r2qk2r/ppp1bppp/2n5/3p1b2/3P1Bn1/1QN1P3/PP3P1P/R3KBNR w KQkq - bm Qxd5; id "ERET 008 - Center";
rnb1kb1r/p4p2/1qp1pn2/1p2N2p/2p1P1p1/2N3B1/PPQ1BPPP/3RK2R w Kkq - bm Ng6; id "ERET 009 - Knight Sacrifice";
5rk1/pp1b4/4pqp1/2Ppb2p/1P2p3/4Q2P/P3BPP1/1R3R1K b - - bm d4; id "ERET 010 - Passed Pawn";
r1b2r1k/ppp2ppp/8/4p3/2BPQ3/P3P1K1/1B3PPP/n3q1NR w - - bm dxe5, Nf3; id "ERET 011 - Attacking Castle";
1nkr1b1r/5p2/1q2p2p/1ppbP1p1/2pP4/2N3B1/1P1QBPPP/R4RK1 w - - bm Nxd5; id "ERET 012 - Relief";
1nrq1rk1/p4pp1/bp2pn1p/3p4/2PP1B2/P1PB2N1/4QPPP/1R2R1K1 w - - bm Qd2, Bc2; id "ERET 013 - Center";
5k2/1rn2p2/3pb1p1/7p/p3PP2/PnNBK2P/3N2P1/1R6 w - - bm Nf3; id "ERET 014 - Endgame";
8/p2p4/r7/1k6/8/pK5Q/P7/b7 w - - bm Qd3; id "ERET 015 - Endgame";
1b1rr1k1/pp1q1pp1/8/NP1p1b1p/1B1Pp1n1/PQR1P1P1/4BP1P/5RK1 w - - bm Nc6; id "ERET 016 - Pos. Sacrifice";
1r3rk1/6p1/p1pb1qPp/3p4/4nPR1/2N4Q/PPP4P/2K1BR2 b - - bm Rxb2; id "ERET 017 - King Attack";
r1b1kb1r/1p1n1p2/p3pP1p/q7/3N3p/2N5/P1PQB1PP/1R3R1K b kq - bm Qg5; id "ERET 018 - Development";
3kB3/5K2/7p/3p4/3pn3/4NN2/8/1b4B1 w - - bm Nf5; id "ERET 019 - Endgame";
1nrrb1k1/1qn1bppp/pp2p3/3pP3/N2P3P/1P1B1NP1/PBR1QPK1/2R5 w - - bm Bxh7; id "ERET 020 - Bishop Sacrifice h7";
3rr1k1/1pq2b1p/2pp2p1/4bp2/pPPN4/4P1PP/P1QR1PB1/1R4K1 b - - bm Rc8; id "ERET 021 - Prophylaxis";
r4rk1/p2nbpp1/2p2np1/q7/Np1PPB2/8/PPQ1N1PP/1K1R3R w - - bm h4; id "ERET 022 - Passed Pawn";
r3r2k/1bq1nppp/p2b4/1pn1p2P/2p1P1QN/2P1N1P1/PPBB1P1R/2KR4 w - - bm Ng6; id "ERET 023 - Attacking Castle";
r2q1r1k/3bppbp/pp1p4/2pPn1Bp/P1P1P2P/2N2P2/1P1Q2P1/R3KB1R w KQ - am b3; id "ERET 024 - Development";
2kb4/p7/r1p3p1/p1P2pBp/R2P3P/2K3P1/5P2/8 w - - bm Bxd8; id "ERET 025 - Endgame";
rqn2rk1/pp2b2p/2n2pp1/1N2p3/5P1N/1PP1B3/4Q1PP/R4RK1 w - - bm Nxg6; id "ERET 026 - Knight Sacrifice";
8/3Pk1p1/1p2P1K1/1P1Bb3/7p/7P/6P1/8 w - - bm g4; id "ERET 027 - Zugzwang";
4rrk1/Rpp3pp/6q1/2PPn3/4p3/2N5/1P2QPPP/5RK1 w - - am Rxb7; id "ERET 028 - Poisoned Pawn";
2q2rk1/2p2pb1/PpP1p1pp/2n5/5B1P/3Q2P1/4PPN1/2R3K1 w - - bm Rxc5; id "ERET 029 - Exchange Sacrifice";
rnbq1r1k/4p1bP/p3p3/1pn5/8/2Np1N2/PPQ2PP1/R1B1KB1R w KQ - bm Nh4; id "ERET 030 - Initiative";
4b1k1/1p3p2/4pPp1/p2pP1P1/P2P4/1P1B4/8/2K5 w - - bm b4; id "ERET 031 - Endgame";
8/7p/5P1k/1p5P/5p2/2p1p3/P1P1P1P1/1K3Nb1 w - - bm Ng3; id "ERET 032 - Zugzwang";
r3kb1r/ppnq2pp/2n5/4pp2/1P1PN3/P4N2/4QPPP/R1B1K2R w KQkq - bm Nxe5; id "ERET 033 - Initiative";
b4r1k/6bp/3q1ppN/1p2p3/3nP1Q1/3BB2P/1P3PP1/2R3K1 w - - bm Rc8; id "ERET 034 - Bishop Pair";
r3k2r/5ppp/3pbb2/qp1Np3/2BnP3/N7/PP1Q1PPP/R3K2R w KQkq - bm Nxb5; id "ERET 035 - Exchange Sacrifice";
r1k1n2n/8/pP6/5R2/8/1b1B4/4N3/1K5N w - - bm b7; id "ERET 036 - Endgame";
1k6/bPN2pp1/Pp2p3/p1p5/2pn4/3P4/PPR5/1K6 w - - bm Na8; id "ERET 037 - Zugzwang";
8/6N1/3kNKp1/3p4/4P3/p7/P6b/8 w - - bm exd5; id "ERET 038 - Endgame";
r1b1k2r/pp3ppp/1qn1p3/2bn4/8/6P1/PPN1PPBP/RNBQ1RK1 w kq - bm a3; id "ERET 039 - Development";
r3kb1r/3n1ppp/p3p3/1p1pP2P/P3PBP1/4P3/1q2B3/R2Q1K1R b kq - bm Bc5; id "ERET 040 - King Safety";
3q1rk1/2nbppb1/pr1p1n1p/2pP1Pp1/2P1P2Q/2N2N2/1P2B1PP/R1B2RK1 w - - bm Nxg5; - id "ERET 041 - Knight Sacrifice";
8/2k5/N3p1p1/2KpP1P1/b2P4/8/8/8 b - - bm Kb7; id "ERET 042 - Endgame";
2r1rbk1/1pqb1p1p/p2p1np1/P4p2/3NP1P1/2NP1R1Q/1P5P/R5BK w - - bm Nxf5; id "ERET 043 - Knight Sacrifice";
rnb2rk1/pp2q2p/3p4/2pP2p1/2P1Pp2/2N5/PP1QBRPP/R5K1 w - - bm h4; id "ERET 044 - Open Line";
5rk1/p1p1rpb1/q1Pp2p1/3Pp2p/4Pn2/1R4N1/P1BQ1PPP/R5K1 w - - bm Rb4; id "ERET 045 - Initiative";
8/4nk2/1p3p2/1r1p2pp/1P1R1N1P/6P1/3KPP2/8 w - - bm Nd3; id "ERET 046 - Endgame";
4kbr1/1b1nqp2/2p1p3/2N4p/1p1PP1pP/1PpQ2B1/4BPP1/r4RK1 w - - bm Nxb7; id "ERET 047 - Relief";
r1b2rk1/p2nqppp/1ppbpn2/3p4/2P5/1PN1PN2/PBQPBPPP/R4RK1 w - - bm cxd5; id "ERET 048 - Stong Squares";
r1b1kq1r/1p1n2bp/p2p2p1/3PppB1/Q1P1N3/8/PP2BPPP/R4RK1 w kq - bm f4; id "ERET 049 - Development";
r4r1k/p1p3bp/2pp2p1/4nb2/N1P4q/1P5P/PBNQ1PP1/R4RK1 b - - bm Nf3; id "ERET 050 - King Attack";
6k1/pb1r1qbp/3p1p2/2p2p2/2P1rN2/1P1R3P/PB3QP1/3R2K1 b - - bm Bh6; id "ERET 051 - Defence";
2r2r2/1p1qbkpp/p2ppn2/P1n1p3/4P3/2N1BB2/QPP2PPP/R4RK1 w - - bm b4; id "ERET 052 - Stong Squares";
r1bq1rk1/p4ppp/3p2n1/1PpPp2n/4P2P/P1PB1PP1/2Q1N3/R1B1K2R b KQ - bm c4; id "ERET 053 - Pos. Sacrifice";
2b1r3/5pkp/6p1/4P3/QppqPP2/5RPP/6BK/8 b - - bm c3; id "ERET 054 - Endgame";
r2q1rk1/1p2bpp1/p1b2n1p/8/5B2/2NB4/PP1Q1PPP/3R1RK1 w - - bm Bxh6; id "ERET 055 - Bishop Sacrifice h6";
r2qr1k1/pp2bpp1/2pp3p/4nbN1/2P4P/4BP2/PPPQ2P1/1K1R1B1R w - - bm Be2; id "ERET 056 - Zwischenzug";
r2qr1k1/pp1bbp2/n5p1/2pPp2p/8/P2PP1PP/1P2N1BK/R1BQ1R2 w - - bm d6; id "ERET 057 - Exchange";
8/8/R7/1b4k1/5p2/1B3r2/7P/7K w - - bm h4; id "ERET 058 - Endgame";
rq6/5k2/p3pP1p/3p2p1/6PP/1PB1Q3/2P5/1K6 w - - bm Qd3; id "ERET 059 - Endgame";
q2B2k1/pb4bp/4p1p1/2p1N3/2PnpP2/PP3B2/6PP/2RQ2K1 b - - bm Qxd8; id "ERET 060 - King Attack";
4rrk1/pp4pp/3p4/3P3b/2PpPp1q/1Q5P/PB4B1/R4RK1 b - - bm Rf6; id "ERET 061 - King Attack";
rr1nb1k1/2q1b1pp/pn1p1p2/1p1PpNPP/4P3/1PP1BN2/2B2P2/R2QR1K1 w - - bm g6; id "ERET 062 - Stong Squares";
r3k2r/4qn2/p1p1b2p/6pB/P1p5/2P5/5PPP/RQ2R1K1 b kq - bm Kf8; id "ERET 063 - Defence";
8/1pp5/p3k1pp/8/P1p2PPP/2P2K2/1P3R2/5r2 b - - am Rxf2; id "ERET 064 - Endgame";
1r3rk1/2qbppbp/3p1np1/nP1P2B1/2p2P2/2N1P2P/1P1NB1P1/R2Q1RK1 b - - bm Qb6; id "ERET 065 - Zwischenzug";
8/2pN1k2/p4p1p/Pn1R4/3b4/6Pp/1P3K1P/8 w - - bm Ke1; id "ERET 066 - Endgame";
5r1k/1p4bp/3p1q2/1NpP1b2/1pP2p2/1Q5P/1P1KBP2/r2RN2R b - - bm f3; id "ERET 067 - Clearance";
r3kb1r/pbq2ppp/1pn1p3/2p1P3/1nP5/1P3NP1/PB1N1PBP/R2Q1RK1 w kq - bm a3; id "ERET 068 - Open Line";
5rk1/n2qbpp1/pp2p1p1/3pP1P1/PP1P3P/2rNPN2/R7/1Q3RK1 w - - bm h5; id "ERET 069 - King Attack";
r5k1/1bqp1rpp/p1n1p3/1p4p1/1b2PP2/2NBB1P1/PPPQ4/2KR3R w - - bm a3; id "ERET 070 - Stong Squares";
1r4k1/1nq3pp/pp1pp1r1/8/PPP2P2/6P1/5N1P/2RQR1K1 w - - bm f5; id "ERET 071 - Deflection";
q5k1/p2p2bp/1p1p2r1/2p1np2/6p1/1PP2PP1/P2PQ1KP/4R1NR b - - bm Qd5; id "ERET 072 - Centralization";
r4rk1/ppp2ppp/1nnb4/8/1P1P3q/PBN1B2P/4bPP1/R2QR1K1 w - - bm Qxe2; id "ERET 073 - Mobility";
1r3k2/2N2pp1/1pR2n1p/4p3/8/1P1K1P2/P5PP/8 w - - bm Kc4; id "ERET 074 - Endgame";
6r1/6r1/2p1k1pp/p1pbP2q/Pp1p1PpP/1P1P2NR/1KPQ3R/8 b - - bm Qf5; id "ERET 075 - Fortress";
r1b1kb1r/1p1npppp/p2p1n2/6B1/3NPP2/q1N5/P1PQ2PP/1R2KB1R w Kkq - bm Bxf6; id "ERET 076 - Development";
r3r1k1/1bq2ppp/p1p2n2/3ppPP1/4P3/1PbB4/PBP1Q2P/R4R1K w - - bm gxf6; id "ERET 077 - Attacking Castle";
r4rk1/ppq3pp/2p1Pn2/4p1Q1/8/2N5/PP4PP/2KR1R2 w - - bm Rxf6; id "ERET 078 - Passed Pawn";
r1bqr1k1/3n1ppp/p2p1b2/3N1PP1/1p1B1P2/1P6/1PP1Q2P/2KR2R1 w - - bm Qxe8; id "ERET 079 - Queen Sacrifice";
5rk1/1ppbq1pp/3p3r/pP1PppbB/2P5/P1BP4/5PPP/3QRRK1 b - - bm Bc1; id "ERET 080 - Clearance";
r3r1kb/p2bp2p/1q1p1npB/5NQ1/2p1P1P1/2N2P2/PPP5/2KR3R w - - bm Bg7; id "ERET 081 - King Attack";
8/3P4/1p3b1p/p7/P7/1P3NPP/4p1K1/3k4 w - - bm g4; id "ERET 082 - Endgame";
3q1rk1/7p/rp1n4/p1pPbp2/P1P2pb1/1QN4P/1B2B1P1/1R3RK1 w - - bm Nb5; id "ERET 083 - Exchange";
4r1k1/1r1np3/1pqp1ppB/p7/2b1P1PQ/2P2P2/P3B2R/3R2K1 w - - bm Bg7 Bg5; id "ERET 084 - King Attack";
r4rk1/q4bb1/p1R4p/3pN1p1/8/2N3P1/P4PP1/3QR1K1 w - - bm Ng4; id "ERET 085 - Exchange";
r3k2r/pp2pp1p/8/q2Pb3/2P5/4p3/B1Q2PPP/2R2RK1 w kq - bm c5; id "ERET 086 - Exchange Sacrifice";
r3r1k1/1bnq1pbn/p2p2p1/1p1P3p/2p1PP1B/P1N2B1P/1PQN2P1/3RR1K1 w - - bm e5; id "ERET 087 - Clearance";
8/4k3/p2p2p1/P1pPn2p/1pP1P2P/1P1NK1P1/8/8 w - - bm g4; id "ERET 088 - Endgame";
8/2P1P3/b1B2p2/1pPRp3/2k3P1/P4pK1/nP3p1p/N7 w - - bm e8N; id "ERET 089 - Underpromotion";
4K1k1/8/1p5p/1Pp3b1/8/1P3P2/P1B2P2/8 w - - bm f4; id "ERET 090 - Endgame";
8/6p1/3k4/3p1p1p/p2K1P1P/4P1P1/P7/8 b - - bm g6, Kc6; id "ERET 091 - Endgame";
r1b2rk1/ppp3p1/4p2p/4Qpq1/3P4/2PB4/PPK2PPP/R6R b - - am Qxg2; id "ERET 092 - Poisoned Pawn";
2b1r3/r2ppN2/8/1p1p1k2/pP1P4/2P3R1/PP3PP1/2K5 w - - bm Nd6; id "ERET 093 - Endgame";
2k2Br1/p6b/Pq1r4/1p2p1b1/1Ppp2p1/Q1P3N1/5RPP/R3N1K1 b - - bm Rf6; id "ERET 094 - Queen Sacrifice";
r2qk2r/ppp1b1pp/2n1p3/3pP1n1/3P2b1/2PB1NN1/PP4PP/R1BQK2R w KQkq - bm Nxg5; id "ERET 095 - Queen Sacrifice";
8/8/4p1Pk/1rp1K1p1/4P1P1/1nP2Q2/p2b1P2/8 w - - bm Kf6; id "ERET 096 - Endgame";
2k5/p7/Pp1p1b2/1P1P1p2/2P2P1p/3K3P/5B2/8 w - - bm c5; id "ERET 097 - Endgame";
8/6pp/5k2/1p1r4/4R3/7P/5PP1/5K2 w - - am Ke2; id "ERET 098 - Endgame";
3q1r1k/4RPp1/p6p/2pn4/2P5/1P6/P3Q2P/6K1 w - - bm Re8; id "ERET 099 - Endgame";
rn2k2r/3pbppp/p3p3/8/Nq1Nn3/4B1P1/PP3P1P/R2Q1RK1 w k - bm Nf5; id "ERET 100 - Initiative";
r1b1kb1N/pppnq1pB/8/3p4/3P4/8/PPPK1nPP/RNB1R3 b q - bm Ne5; id "ERET 101 - Development";
N4rk1/pp1b1ppp/n3p1n1/3pP1Q1/1P1N4/8/1PP2PPP/q1B1KB1R b K - bm Nxb4; id "ERET 102 - King Attack";
4k1br/1K1p1n1r/2p2pN1/P2p1N2/2P3pP/5B2/P2P4/8 w - - bm Kc8; id "ERET 103 - Zugzwang";
r1bqkb1r/ppp3pp/2np4/3N1p2/3pnB2/5N2/PPP1QPPP/2KR1B1R b kq - bm Ne7; id "ERET 104 - Development";
r3kb1r/pbqp1pp1/1pn1pn1p/8/3PP3/2PB1N2/3N1PPP/R1BQR1K1 w kq - bm e5; id "ERET 105 - Stong Squares";
r2r2k1/pq2bppp/1np1bN2/1p2B1P1/5Q2/P4P2/1PP4P/2KR1B1R b - - bm Bxf6; id "ERET 106 - King Safety";
1r1r2k1/2pq3p/4p3/2Q1Pp2/1PNn1R2/P5P1/5P1P/4R2K b - - bm Rb5; id "ERET 107 - Defence";
8/5p1p/3P1k2/p1P2n2/3rp3/1B6/P4R2/6K1 w - - bm Ba4; id "ERET 108 - Endgame";
2rbrnk1/1b3p2/p2pp3/1p4PQ/1PqBPP2/P1NR4/2P4P/5RK1 b - - bm Qxd4; id "ERET 109 - Relief";
4r1k1/1bq2r1p/p2p1np1/3Pppb1/P1P5/1N3P2/1R2B1PP/1Q1R2BK w - - bm c5; id "ERET 110 - Passed Pawn";
8/8/8/8/4kp2/1R6/P2q1PPK/8 w - - bm a3; id "ERET 111 - Fortress";"##;


/// Silent but Deadly, an EPD test by Dann Corbit that is designed to help tune chess engines.
pub static SILENT_BUT_DEADLY: &str = r##"
1qr3k1/p2nbppp/bp2p3/3p4/3P4/1P2PNP1/P2Q1PBP/1N2R1K1 b - - bm Qc7; id "sbd.001";
1r2r1k1/3bnppp/p2q4/2RPp3/4P3/6P1/2Q1NPBP/2R3K1 w - - bm Rc7; id "sbd.002";
2b1k2r/2p2ppp/1qp4n/7B/1p2P3/5Q2/PPPr2PP/R2N1R1K b k - bm O-O; id "sbd.003";
2b5/1p4k1/p2R2P1/4Np2/1P3Pp1/1r6/5K2/8 w - - bm Rd8; id "sbd.004";
2brr1k1/ppq2ppp/2pb1n2/8/3NP3/2P2P2/P1Q2BPP/1R1R1BK1 w - - bm g3; id "sbd.005";
2kr2nr/1pp3pp/p1pb4/4p2b/4P1P1/5N1P/PPPN1P2/R1B1R1K1 b - - bm Bf7; id "sbd.006";
2r1k2r/1p1qbppp/p3pn2/3pBb2/3P4/1QN1P3/PP2BPPP/2R2RK1 b k - bm O-O; id "sbd.007";
2r1r1k1/pbpp1npp/1p1b3q/3P4/4RN1P/1P4P1/PB1Q1PB1/2R3K1 w - - bm Rce1; id "sbd.008";
2r2k2/r4p2/2b1p1p1/1p1p2Pp/3R1P1P/P1P5/1PB5/2K1R3 w - - bm Kd2; id "sbd.009";
2r3k1/5pp1/1p2p1np/p1q5/P1P4P/1P1Q1NP1/5PK1/R7 w - - bm Rd1; id "sbd.010";
2r3qk/p5p1/1n3p1p/4PQ2/8/3B4/5P1P/3R2K1 w - - bm e6; id "sbd.011";
3b4/3k1pp1/p1pP2q1/1p2B2p/1P2P1P1/P2Q3P/4K3/8 w - - bm Qf3; id "sbd.012";
3n1r1k/2p1p1bp/Rn4p1/6N1/3P3P/2N1B3/2r2PP1/5RK1 w - - bm Na4 Nce4; id "sbd.013";
3q1rk1/3rbppp/ppbppn2/1N6/2P1P3/BP6/P1B1QPPP/R3R1K1 w - - bm Nd4; id "sbd.014";
3r1rk1/p1q4p/1pP1ppp1/2n1b1B1/2P5/6P1/P1Q2PBP/1R3RK1 w - - bm Bh6; id "sbd.015";
3r2k1/2q2p1p/5bp1/p1pP4/PpQ5/1P3NP1/5PKP/3R4 b - - bm Qd6; id "sbd.016";
3r2k1/p1q1npp1/3r1n1p/2p1p3/4P2B/P1P2Q1P/B4PP1/1R2R1K1 w - - bm Bc4; id "sbd.017";
3r4/2k5/p3N3/4p3/1p1p4/4r3/PPP3P1/1K1R4 b - - bm Kd7; id "sbd.018";
3r4/2R1np1p/1p1rpk2/p2b1p2/8/PP2P3/4BPPP/2R1NK2 w - - bm b4; id "sbd.019";
3rk2r/1b2bppp/p1qppn2/1p6/4P3/PBN2PQ1/1PP3PP/R1B1R1K1 b k - bm O-O; id "sbd.020";
3rk2r/1bq2pp1/2pb1n1p/p3pP2/P1B1P3/8/1P2QBPP/2RN1R1K b k - bm Be7 O-O; id "sbd.021";
3rkb1r/pppb1pp1/4n2p/2p5/3NN3/1P5P/PBP2PP1/3RR1K1 w - - bm Nf5; id "sbd.022";
3rr1k1/1pq2ppp/p1n5/3p4/6b1/2P2N2/PP1QBPPP/3R1RK1 w - - bm Rfe1; id "sbd.023";
4r1k1/1q1n1ppp/3pp3/rp6/p2PP3/N5P1/PPQ2P1P/3RR1K1 w - - bm Rc1; id "sbd.024";
4rb1k/1bqn1pp1/p3rn1p/1p2pN2/1PP1p1N1/P1P2Q1P/1BB2PP1/3RR1K1 w - - bm Qe2; id "sbd.025";
4rr2/2p5/1p1p1kp1/p6p/P1P4P/6P1/1P3PK1/3R1R2 w - - bm Rfe1; id "sbd.026";
5r2/pp1b1kpp/8/2pPp3/2P1p2P/4P1r1/PPRKB1P1/6R1 b - - bm Ke7; id "sbd.027";
6k1/1R5p/r2p2p1/2pN2B1/2bb4/P7/1P1K2PP/8 w - - bm Nf6+; id "sbd.028";
6k1/pp1q1pp1/2nBp1bp/P2pP3/3P4/8/1P2BPPP/2Q3K1 w - - bm Qc5; id "sbd.029";
6k1/pp2rp1p/2p2bp1/1n1n4/1PN3P1/P2rP2P/R3NPK1/2B2R2 w - - bm Rd2; id "sbd.030";
8/2p2kpp/p6r/4Pp2/1P2pPb1/2P3P1/P2B1K1P/4R3 w - - bm h4; id "sbd.031";
Q5k1/5pp1/5n1p/2b2P2/8/5N1P/5PP1/2q1B1K1 b - - bm Kh7; id "sbd.032";
r1b1k1nr/1p3ppp/p1np4/4p1q1/2P1P3/N1NB4/PP3PPP/2RQK2R w Kkq - bm O-O; id "sbd.033";
r1b1k2r/p1pp1ppp/1np1q3/4P3/1bP5/1P6/PB1NQPPP/R3KB1R b KQkq - bm O-O; id "sbd.034";
r1b1k2r/ppppqppp/8/2bP4/3p4/6P1/PPQPPPBP/R1B2RK1 b kq - bm O-O; id "sbd.035";
r1b1k2r/ppq1bppp/2n5/2N1p3/8/2P1B1P1/P3PPBP/R2Q1RK1 b kq - bm O-O; id "sbd.036";
r1b1kb1r/pp2qppp/2pp4/8/4nP2/2N2N2/PPPP2PP/R1BQK2R w KQkq - bm O-O; id "sbd.037";
r1b1qrk1/pp4b1/2pRn1pp/5p2/2n2B2/2N2NPP/PPQ1PPB1/5RK1 w - - bm Rd3; id "sbd.038";
r1b2rk1/1pqn1pp1/p2bpn1p/8/3P4/2NB1N2/PPQB1PPP/3R1RK1 w - - bm Rc1; id "sbd.039";
r1b2rk1/2qnbp1p/p1npp1p1/1p4PQ/4PP2/1NNBB3/PPP4P/R4RK1 w - - bm Qh6; id "sbd.040";
r1b2rk1/pp2ppbp/2n2np1/2q5/5B2/1BN1PN2/PP3PPP/2RQK2R w K - bm O-O; id "sbd.041";
r1b2rk1/pp4pp/1q1Nppn1/2n4B/1P3P2/2B2RP1/P6P/R2Q3K b - - bm Na6; id "sbd.042";
r1b2rk1/ppp1qppp/1b1n4/8/B2n4/3NN3/PPPP1PPP/R1BQK2R w KQ - bm O-O; id "sbd.043";
r1b2rk1/ppq1bppp/2p1pn2/8/2NP4/2N1P3/PP2BPPP/2RQK2R w K - bm O-O; id "sbd.044";
r1bq1rk1/1p1n1pp1/p4n1p/2bp4/8/2NBPN2/PPQB1PPP/R3K2R w KQ - bm O-O; id "sbd.045";
r1bq1rk1/1p2ppbp/p2p1np1/6B1/2P1P3/2N5/PP1QBPPP/R3K2R w KQ - bm O-O; id "sbd.046";
r1bq1rk1/1p3ppp/p1np4/3Np1b1/2B1P3/P7/1PP2PPP/RN1QK2R w KQ - bm O-O; id "sbd.047";
r1bq1rk1/4bppp/ppnppn2/8/2P1P3/2N5/PPN1BPPP/R1BQK2R w KQ - bm O-O; id "sbd.048";
r1bq1rk1/pp1n1pbp/2n1p1p1/2ppP3/8/2PP1NP1/PP1N1PBP/R1BQ1RK1 w - - bm d4; id "sbd.049";
r1bq1rk1/pp1pppbp/2n2np1/8/4P3/1NN5/PPP1BPPP/R1BQK2R w KQ - bm O-O; id "sbd.050";
r1bq1rk1/pp2ppbp/2n2np1/2p3B1/4P3/2P2N2/PP1NBPPP/R2QK2R w KQ - bm O-O; id "sbd.051";
r1bq1rk1/pp2ppbp/2n3p1/2p5/2BPP3/2P1B3/P3NPPP/R2QK2R w KQ - bm O-O; id "sbd.052";
r1bq1rk1/pp3ppp/2n1pn2/2p5/1bBP4/2N1PN2/PP3PPP/R1BQ1RK1 w - - bm a3; id "sbd.053";
r1bq1rk1/pp3ppp/2n2n2/3p4/8/P1NB4/1PP2PPP/R1BQK2R w KQ - bm O-O; id "sbd.054";
r1bq1rk1/ppp1npb1/3p2pp/3Pp2n/1PP1P3/2N5/P2NBPPP/R1BQR1K1 b - - bm Nf4; id "sbd.055";
r1bq1rk1/ppp2ppp/2n1pn2/3p4/1bPP4/2NBPN2/PP3PPP/R1BQK2R w KQ - bm O-O; id "sbd.056";
r1bq1rk1/pppp1pbp/2n2np1/4p3/2P5/P1N2NP1/1P1PPPBP/R1BQK2R w KQ - bm O-O; id "sbd.057";
r1bqk2r/2ppbppp/p1n2n2/1p2p3/4P3/1B3N2/PPPPQPPP/RNB2RK1 b kq - bm O-O; id "sbd.058";
r1bqk2r/5ppp/p1np4/1p1Np1b1/4P3/2P5/PPN2PPP/R2QKB1R b KQkq - bm O-O; id "sbd.059";
r1bqk2r/bp3ppp/p1n1pn2/3p4/1PP5/P1N1PN2/1B3PPP/R2QKB1R b KQkq - bm O-O; id "sbd.060";
r1bqk2r/p2pppbp/2p3pn/2p5/4P3/2P2N2/PP1P1PPP/RNBQR1K1 b kq - bm O-O; id "sbd.061";
r1bqk2r/pp2bppp/2n1p3/1B1n4/3P4/2N2N2/PP3PPP/R1BQ1RK1 b kq - bm O-O; id "sbd.062";
r1bqk2r/pp2bppp/2n1p3/3n4/3P4/2NB1N2/PP3PPP/R1BQ1RK1 b kq - bm O-O; id "sbd.063";
r1bqk2r/pp2ppbp/2np1np1/2p5/4P3/1B1P1N1P/PPP2PP1/RNBQK2R w KQkq - bm O-O; id "sbd.064";
r1bqk2r/ppn1bppp/2n5/2p1p3/8/2NP1NP1/PP1BPPBP/R2Q1RK1 b kq - bm O-O; id "sbd.065";
r1bqk2r/ppp1bppp/2n5/3p4/3P4/2PB1N2/P1P2PPP/R1BQ1RK1 b kq - bm O-O; id "sbd.066";
r1bqk2r/ppp2ppp/2nb4/3np3/8/PP2P3/1BQP1PPP/RN2KBNR b KQkq - bm O-O; id "sbd.067";
r1bqk2r/ppp2ppp/3b4/4p3/8/1PPP1N2/2PB1PPP/R2Q1RK1 b kq - bm O-O; id "sbd.068";
r1bqk2r/pppp1ppp/5n2/4p3/Bb2P3/5Q2/PPPPNPPP/R1B1K2R b KQkq - bm O-O; id "sbd.069";
r1bqkb1r/pp3ppp/2n5/2pp4/3Pn3/2N2N2/PPP1BPPP/R1BQK2R w KQkq - bm O-O; id "sbd.070";
r1bqkb1r/pp3ppp/2npp3/3nP3/2BP4/5N2/PP3PPP/RNBQK2R w KQkq - bm O-O; id "sbd.071";
r1bqkbnr/3p1ppp/p1p1p3/8/4P3/3B4/PPP2PPP/RNBQK2R w KQkq - bm O-O; id "sbd.072";
r1bqkbnr/ppp2ppp/2n5/8/2BpP3/5N2/PP3PPP/RNBQK2R w KQkq - bm O-O; id "sbd.073";
r1bqrbk1/1pp3pp/2n2p2/p2np3/8/PP1PPN2/1BQNBPPP/R3K2R w KQ - bm O-O; id "sbd.074";
r1br2k1/1p2qppp/pN2pn2/P7/2pn4/4N1P1/1P2PPBP/R3QRK1 b - - bm Rb8; id "sbd.075";
r1q1k2r/1b1nbppp/pp1ppn2/8/2PQP3/1PN2NP1/PB3PBP/R2R2K1 b kq - bm O-O; id "sbd.076";
r1q1k2r/pb1nbppp/1p2pn2/8/P1PNP3/2B3P1/2QN1PBP/R4RK1 b kq - bm O-O; id "sbd.077";
r1r3k1/1bq2pbp/pp1pp1p1/2n5/P3PP2/R2B4/1PPBQ1PP/3N1R1K w - - bm Bc3; id "sbd.078";
r1rn2k1/pp1qppbp/6p1/3pP3/3P4/1P3N1P/PB1Q1PP1/R3R1K1 w - - bm Rac1; id "sbd.079";
r2q1rk1/1b1nbpp1/pp2pn1p/8/2BN3B/2N1P3/PP2QPPP/2R2RK1 w - - bm Rfd1; id "sbd.080";
r2q1rk1/1b3ppp/4pn2/1pP5/1b6/2NBPN2/1PQ2PPP/R3K2R w KQ - bm O-O; id "sbd.081";
r2q1rk1/pb1nppbp/6p1/1p6/3PP3/3QBN1P/P3BPP1/R3K2R w KQ - bm O-O; id "sbd.082";
r2q1rk1/pb2bppp/npp1pn2/3pN3/2PP4/1PB3P1/P2NPPBP/R2Q1RK1 w - - bm e4; id "sbd.083";
r2q1rk1/pppb1pbp/2np1np1/4p3/2P5/P1NPPNP1/1P3PBP/R1BQK2R w KQ - bm O-O; id "sbd.084";
r2qk2r/1b1n1ppp/4pn2/p7/1pPP4/3BPN2/1B3PPP/R2QK2R w KQkq - bm O-O; id "sbd.085";
r2qk2r/1b2bppp/p1n1pn2/1p6/1P6/P2BPN2/1B2QPPP/RN3RK1 b kq - bm O-O; id "sbd.086";
r2qk2r/2p2ppp/p1n1b3/1pbpP3/4n3/1BP2N2/PP1N1PPP/R1BQ1RK1 b kq - bm O-O; id "sbd.087";
r2qk2r/3n1ppp/p3p3/3nP3/3R4/5N2/1P1N1PPP/3QR1K1 b kq - bm O-O; id "sbd.088";
r2qk2r/p1pn1ppp/b3pn2/3p4/Nb1P4/1P3NP1/P3PPBP/1RBQ1RK1 b kq - bm O-O Qe7; id "sbd.089";
r2qk2r/ppp1bppp/2n2n2/8/2BP2b1/2N2N2/PP3PPP/R1BQR1K1 b kq - bm O-O; id "sbd.090";
r2qkb1r/pb1n1p2/2p1p2p/4P1pn/PppP4/2N2NB1/1P2BPPP/R2Q1RK1 w kq - bm Ne4; id "sbd.091";
r2qkb1r/pp2nppp/1np1p3/4Pb2/3P4/PB3N2/1P3PPP/RNBQ1RK1 b kq - bm Ned5; id "sbd.092";
r2qkb1r/pp3ppp/2bppn2/8/2PQP3/2N2N2/PP3PPP/R1B1K2R w KQkq - bm O-O; id "sbd.093";
r2qr1k1/p3bppp/1p2n3/3Q1N2/5P2/4B1P1/PP3R1P/R5K1 w - - bm Rd1; id "sbd.094";
r2r2k1/p1pnqpp1/1p2p2p/3b4/3P4/3BPN2/PP3PPP/2RQR1K1 b - - bm c5; id "sbd.095";
r2r2k1/pp1b1ppp/8/3p2P1/3N4/P3P3/1P3P1P/3RK2R b K - bm Rac8; id "sbd.096";
r3k2r/1b1nb1p1/p1q1pn1p/1pp3N1/4PP2/2N5/PPB3PP/R1BQ1RK1 w kq - bm Nf3; id "sbd.097";
r3k2r/1pqnnppp/p5b1/1PPp1p2/3P4/2N5/P2NB1PP/2RQ1RK1 b kq - bm O-O; id "sbd.098";
r3k2r/p1q1nppp/1pn5/2P1p3/4P1Q1/P1P2P2/4N1PP/R1B2K1R b kq - bm O-O; id "sbd.099";
r3k2r/pp2pp1p/6p1/2nP4/1R2PB2/4PK2/P5PP/5bNR w kq - bm Ne2; id "sbd.100";
r3k2r/ppp1bppp/2n5/3n4/3PB3/8/PP3PPP/RNB1R1K1 b kq - bm O-O-O; id "sbd.101";
r3kb1r/pp3ppp/4bn2/3p4/P7/4N1P1/1P2PPBP/R1B1K2R w KQkq - bm O-O; id "sbd.102";
r3kbnr/1pp3pp/p1p2p2/8/3qP3/5Q1P/PP3PP1/RNB2RK1 w kq - bm Rd1; id "sbd.103";
r3kr2/pppb1p2/2n3p1/3Bp2p/4P2N/2P5/PP3PPP/2KR3R b q - bm O-O-O; id "sbd.104";
r3nrk1/pp2qpb1/3p1npp/2pPp3/2P1P2N/2N3Pb/PP1BBP1P/R2Q1RK1 w - - bm Re1; id "sbd.105";
r3r1k1/1pqn1pbp/p2p2p1/2nP2B1/P1P1P3/2NB3P/5PP1/R2QR1K1 w - - bm Rc1; id "sbd.106";
r3r1k1/pp1q1ppp/2p5/P2n1p2/1b1P4/1B2PP2/1PQ3PP/R1B2RK1 w - - bm e4; id "sbd.107";
r3r1k1/pp3ppp/2ppqn2/5R2/2P5/2PQP1P1/P2P2BP/5RK1 w - - bm Qd4; id "sbd.108";
r3rbk1/p2b1p2/5p1p/1q1p4/N7/6P1/PP1BPPBP/3Q1RK1 w - - bm Nc3; id "sbd.109";
r4r1k/pp1bq1b1/n2p2p1/2pPp1Np/2P4P/P1N1BP2/1P1Q2P1/2KR3R w - - bm Ne6; id "sbd.110";
r4rk1/1bqp1ppp/pp2pn2/4b3/P1P1P3/2N2BP1/1PQB1P1P/2R2RK1 w - - bm b3; id "sbd.111";
r4rk1/1q2bppp/p1bppn2/8/3BPP2/3B2Q1/1PP1N1PP/4RR1K w - - bm e5; id "sbd.112";
r4rk1/pp2qpp1/2pRb2p/4P3/2p5/2Q1PN2/PP3PPP/4K2R w K - bm O-O; id "sbd.113";
r7/3rq1kp/2p1bpp1/p1Pnp3/2B4P/PP4P1/1B1RQP2/2R3K1 b - - bm Rad8; id "sbd.114";
r7/pp1bpp2/1n1p2pk/1B3P2/4P1P1/2N5/PPP5/1K5R b - - bm Kg5; id "sbd.115";
rn1q1rk1/p4pbp/bp1p1np1/2pP4/8/P1N2NP1/1PQ1PPBP/R1B1K2R w KQ - bm O-O; id "sbd.116";
rn1q1rk1/pb3p2/1p5p/3n2P1/3p4/P4P2/1P1Q1BP1/R3KBNR b KQ - bm Re8+; id "sbd.117";
rn1q1rk1/pp2bppp/1n2p1b1/8/2pPP3/1BN1BP2/PP2N1PP/R2Q1RK1 w - - bm Bc2; id "sbd.118";
rn1q1rk1/pp3ppp/4bn2/2bp4/5B2/2NBP1N1/PP3PPP/R2QK2R w KQ - bm O-O; id "sbd.119";
rn1qkbnr/pp1b1ppp/8/1Bpp4/3P4/8/PPPNQPPP/R1B1K1NR b KQkq - bm Qe7; id "sbd.120";
rn1qr1k1/pb3p2/1p5p/3n2P1/3p4/P4P2/1P1QNBP1/R3KB1R b KQ - bm d3; id "sbd.121";
rn2kb1r/pp2nppp/1q2p3/3pP3/3P4/5N2/PP2NPPP/R1BQK2R w KQkq - bm O-O; id "sbd.122";
rn3rk1/1bqp1ppp/p3pn2/8/Nb1NP3/4B3/PP2BPPP/R2Q1RK1 w - - bm Rc1; id "sbd.123";
rn3rk1/pbp1qppp/1p1ppn2/8/2PP4/P1Q2NP1/1P2PPBP/R1B1K2R w KQ - bm O-O; id "sbd.124";
rnb1k2r/1pq2ppp/p2ppn2/2b5/3NPP2/2P2B2/PP4PP/RNBQ1R1K b kq - bm O-O; id "sbd.125";
rnb2rk1/ppq1ppbp/6p1/2p5/3PP3/2P2N2/P3BPPP/1RBQK2R w K - bm O-O; id "sbd.126";
rnbq1rk1/5ppp/p3pn2/1p6/2BP4/P1P2N2/5PPP/R1BQ1RK1 w - - bm Bd3; id "sbd.127";
rnbq1rk1/pp2ppbp/2pp1np1/8/P2PP3/2N2N2/1PP1BPPP/R1BQK2R w KQ - bm O-O; id "sbd.128";
rnbq1rk1/ppp1ppbp/6p1/8/8/2P2NP1/P2PPPBP/R1BQK2R w KQ - bm O-O; id "sbd.129";
rnbqk1nr/pp3pbp/2ppp1p1/8/2BPP3/2N2Q2/PPP2PPP/R1B1K1NR w KQkq - bm Nge2; id "sbd.130";
rnbqk2r/ppp2ppp/1b1p1n2/4p3/2B1P3/2PP1N2/PP1N1PPP/R1BQK2R b KQkq - bm O-O; id "sbd.131";
rnbqk2r/pppp2pp/4pn2/5p2/1b1P4/2P2NP1/PP2PPBP/RNBQK2R b KQkq - bm Be7; id "sbd.132";
rnbqr1k1/pp1p1ppp/5n2/3Pb3/1P6/P1N3P1/4NPBP/R1BQK2R w KQ - bm O-O; id "sbd.133";
rnq1nrk1/pp3pbp/6p1/3p4/3P4/5N2/PP2BPPP/R1BQK2R w KQ - bm O-O; id "sbd.134";"##;


