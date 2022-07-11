//! Time control logic.

use std::time::{Instant, Duration};

use crate::Move;

use super::{SearchInfo, SearchEval};


pub enum TimeControl {
    Infinite,
    MoveTime(Duration),
    TimeLeft {
        time_left: Duration, 
        increment: Option<Duration>, 
        moves_left: Option<usize> 
    },
}


pub struct TimeManager {
    /// Total time remaining on the clock.
    pub time_left: Duration,
    /// Time granted per move.
    pub increment: Option<Duration>,
    /// Move count until the next time control.
    pub moves_left: Option<usize>,

    /// Previous search iteration duration in seconds.
    pub prev_iter: Option<Duration>,
    pub avrg_ratio: Option<f64>,
    pub iter_count: usize,

    pub avrg_score: [Option<i16>; 2],

    pub prev_pv: Option<Move>,
    pub pv_concurr_count: usize,

    pub search_begin: Instant,
    pub iter_begin: Instant,
}

impl TimeManager {
    pub fn start(time_left: Duration, increment: Option<Duration>, moves_left: Option<usize>) -> Self {
        Self { 
            time_left, increment, moves_left, 

            prev_iter: None, 
            avrg_ratio: None, 
            iter_count: 0, 
            avrg_score: [None, None], 
            prev_pv: None,
            pv_concurr_count: 0, 
            search_begin: Instant::now(), 
            iter_begin: Instant::now(),
        }
    }

    pub fn iter(&mut self, info: SearchInfo) -> bool {
        let evaluation;
        match info.eval {
            // If a forced mate is found, play immediately
            SearchEval::Mate(_) => return true,
            SearchEval::Normal(eval) => evaluation = eval,
        }

        let t = Instant::now();
        let iter_time = t - self.iter_begin;
        let search_time = t - self.search_begin;


        // update score data
        let index = self.iter_count % 2;
        if let Some(avrg_score) = self.avrg_score[index] {
            let half_iter_count = self.iter_count as i16 / 2;
            let avrg = (avrg_score * half_iter_count + evaluation) / (half_iter_count + 1);
            self.avrg_score[index] = Some(avrg);
        } else {
            self.avrg_score[index] = Some(evaluation);
        }


        // update iteration data
        if let Some(prev_time) = self.prev_iter {
            let ratio = iter_time.as_secs_f64() / prev_time.as_secs_f64();
            if let Some(avrg_ratio) = self.avrg_ratio {
                let iter_count = self.iter_count as f64;
                self.avrg_ratio = Some((avrg_ratio * iter_count + ratio) / (iter_count + 1.0));
            } else {
                self.avrg_ratio = Some(ratio);
            }
            self.prev_iter = Some(iter_time);
            self.iter_count += 1;
        }


        // update pv data
        if let Some(prev_pv) = self.prev_pv {
            if prev_pv == info.best {
                self.pv_concurr_count += 1;
            } else {
                self.pv_concurr_count = 0;
            }
        }
        self.prev_pv = Some(info.best);

        // update time
        self.iter_begin = Instant::now();


        // todo fixme: do the actual thing

        if search_time > todo!() {
            return true;
        }

        false
    }
}

