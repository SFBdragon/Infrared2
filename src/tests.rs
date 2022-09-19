//! Chess search engine tests for benchmarking and iterating.

use std::io::{Write, Read};
use std::time::Duration;
use crossbeam_channel::Receiver;

use infra::{TimeControl, Game, SearchInfo, Board};
use infra::epd::{self, Epd};

#[allow(dead_code)]
pub fn test_pos() {
    let fen = "2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - - ";

    let game = Game::new(Board::from_fen(fen).unwrap(), vec![]).unwrap();
    let handle = game.search(TimeControl::Infinite, 256, None);
    let _ = log_search_info(handle.info_channel.1, &game.position);
}

pub fn log_search_info(rcvr: Receiver<(SearchInfo, bool)>, pos: &Board) -> (Vec<(SearchInfo, Duration)>, Duration) {
    let t1 = std::time::Instant::now();
    let mut prev_time = Duration::ZERO;
    let mut infos = Vec::new();
    while let Ok((info, is_final)) = rcvr.recv() {
        let t2 = std::time::Instant::now();
        let time_taken = t2 - t1;
        let time_during = time_taken - prev_time;
        prev_time = time_taken;

        print!("\nTime: {:12}", time_taken.as_secs_f32());
        print!(" | Depth: {:2}", info.depth.unwrap().0);
        print!(" | Best move: {} {}", info.pv[0].to_lan(pos), match info.eval.unwrap() {
            infra::SearchEval::Normal(score) => (score as f32 / 100.0).to_string(),
            infra::SearchEval::Mate(dist) => "#".to_owned() + dist.to_string().as_str(),
        });

        std::io::stdout().lock().flush().unwrap();
        infos.push((info, time_during));
        if is_final { return (infos, time_taken); }
    }
    unreachable!();
}

pub fn epd_bm_am_assessment(name: &str, epd: &Epd, 
    infos: Vec<(SearchInfo, Duration)>, dur: Duration
) -> (Option<bool>, f64) {
    print!("\n{}", name);
    if let Some(id) = epd.id.as_ref() { print!(" | ID: {}", id.as_str()) }

    let final_move = infos.last().unwrap().0.pv[0];
    print!(" | Final move: {}", final_move.to_lan(&epd.pos));

    let mut status = None;
    if let Some(bm) = epd.bm.as_ref() {
        let is_bm = bm.contains(&final_move);
        status = status.map_or(Some(is_bm), |s| Some(s && is_bm));
        print!(" | Best moves: {:?} [{}]", 
            bm.iter().map(|m| m.to_lan(&epd.pos)).collect::<Vec<_>>(),
            if is_bm { "FOUND" } else { "NOT FOUND" }
        );
    }
    if let Some(am) = epd.am.as_ref() {
        let isnt_am = !am.contains(&final_move);
        status = status.map_or(Some(isnt_am), |s| Some(s && isnt_am));
        print!(" | Avoid moves: {:?} [{}]", 
            am.iter().map(|m| m.to_lan(&epd.pos)).collect::<Vec<_>>(), 
            if isnt_am { "AVOIDED" } else { "NOT AVOIDED" }
        );
    }

    let mut accuracy = 0f64;
    for (info, dur) in infos {
        let iter_move = info.pv[0];
        if let Some(bm) = epd.bm.as_ref() {
            if bm.contains(&iter_move) {
                accuracy += dur.as_secs_f64();
            }
        }
        if let Some(am) = epd.am.as_ref() {
            if am.contains(&iter_move) {
                accuracy -= dur.as_secs_f64();
            }
        }
    }
    accuracy /= dur.as_secs_f64();
    print!(" | Accuracy: {}", accuracy);

    println!();
    (status, accuracy)
}

fn epd_test(name: &str, epds: &str, time_control: TimeControl) {
    println!("{} test", name);

    let mut total = 0;
    let mut score = 0;
    let mut accuracy = 0f64;

    for epd in epd::parse_epd(epds.trim()) {
        let game = Game::new(epd.pos.clone(), vec![]).unwrap();
        let handle = game.search(time_control, 256, None);
        let (infos, dur) = log_search_info(handle.info_channel.1, &epd.pos);
        let (status, acc) = epd_bm_am_assessment(name, &epd, infos, dur);

        if let Some(status) = status {
            if status { score += 1; }
            total += 1;
        }
        accuracy += acc;
    }
    println!("{name} test: Determination: {score} / {0} | Accuracy: {accuracy} / {0}", total);
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
