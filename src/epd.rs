use std::str::Lines;
use crate::{Move, Board};


/// Get an iterator that returns EPD data per-position.
pub fn parse_epd(epd: &str) -> EpdIter {
    EpdIter(epd.lines())
}

#[derive(Debug, Clone)]
pub struct Epd {
    pub pos: Board,
    pub id: Option<String>,
    pub bm: Option<Vec<Move>>,
    pub am: Option<Vec<Move>>,
}

#[derive(Debug)]
pub struct EpdIter<'e>(Lines<'e>);

impl<'a> Iterator for EpdIter<'a> {
    type Item = Epd;

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.0.next()?.trim();
        let mut substrings = line.split_whitespace();
        let mut remaining = line;

        for _ in 0..4 {
            if let Some(ss) = substrings.next() {
                remaining = remaining.trim_start_matches(ss).trim_start();
            } else {
                return self.next();
            }
        }

        let fen = line[..(line.len() - remaining.len())].trim();
        let pos = if let Ok(pos) = Board::from_fen(fen) { pos } else { return self.next() };
        let mut data = Epd { pos, id: None, bm: None, am: None };

        remaining = remaining.trim_start_matches('-').trim_start();
        for (key, val) in remaining.split(';').map_while(|ss| ss.trim().split_once(' ')) {
            match key {
                "id" => data.id = Some(val.trim_matches('"').to_owned()),
                "bm" => data.bm = Some(val.split(',')
                    .filter_map(|san| Move::from_san(san.trim(), &data.pos)).collect()
                ),
                "am" => data.am = Some(val.split(',')
                    .filter_map(|san| Move::from_san(san.trim(), &data.pos)).collect()
                ),
                _    => (),
            }
        }

        Some(data)
    }
}


