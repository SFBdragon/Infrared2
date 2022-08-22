//! Chess search engine tests for benchmarking and iterating.

use std::io::{Write, Read};
use std::time::Duration;
use crossbeam_channel::Receiver;

use infra::{TimeControl, Game, SearchInfo, Board};
use infra::epd::{self, Epd};

#[allow(dead_code)]
pub fn test_pos() {
    let fen = "r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - - 0 1";

    let game = Game::new(Board::from_fen(fen).unwrap(), vec![]).unwrap();
    let handle = game.search(TimeControl::Infinite, None, None);
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
        std::io::stdout().lock().flush().unwrap();
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
    println!("{} test", name);

    let mut total = 0;
    let mut score = 0;

    for epd in epd::parse_epd(epds.trim()) {
        let game = Game::new(epd.pos.clone(), vec![]).unwrap();
        let handle = game.search(time_control, None, None);
        let info = log_search_info(handle.info_channel.1, &epd.pos);
        let status = epd_bm_am_assessment(name, &epd, &info);

        if let Some(status) = status {
            if status { score += 1; }
            total += 1;
        }
    }
    println!("{} test: {} / {}", name, score, total);
}

pub fn epd_test_from_file(path: &std::path::Path, time: Duration) {
    match std::fs::File::open(path) {
        Err(e) => println!("EPD Test path parse error: {:?}", e),
        Ok(mut file) => {
            let mut buf = String::new();
            match file.read_to_string(&mut buf) {
                Err(e) => println!("EPD Test file read error: {:?}", e),
                Ok(_) => {
                    epd_test(
                        path.file_stem().unwrap().to_str().unwrap(), 
                        &buf,
                        TimeControl::MoveTime(time),
                    );
                },
            }
        },
    }
}
