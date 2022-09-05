//! Time control logic.

use std::{cmp, time::{Instant, Duration}};

use crate::Move;

use super::{SearchInfo, SearchEval};

/// Distance from last book move to lend bonus search time to.
const BOOK_DIST: usize = 10;
/// Out-of-book search bonus as a multiple of allocated time.
const BOOK_BIAS: f64 = 1.8;
/// Time bias towards searching longer now, rather than later.
const MOVE_BIAS: f64 = 1.18;
/// Time bias towards searching longer now, rather than later, 
/// when book distance isn't avaialable.
const MOVE_BIAS_NO_BOOK: f64 = 1.24;
/// Maximum ratio of time used to time allocated.
const MAX_TIME_RATIO: f64 = 3.0;
/// Maximum fraction of remaining time used.
const MAX_TIME_LEFT_RATIO: f64 = 0.4;
/// Move count to assume remains when time control does not specify.
const UNKNOWN_MOVES_LEFT: f64 = 25.0;
/// Padding moves onto number of moves left for time partitioning.
const MOVES_LEFT_OFFSET: f64 = 2.0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimeControl {
    Infinite,
    MoveTime(Duration),
    TimeLeft {
        /// Total time remaining on the clock.
        time_left: Duration, 
        /// Time granted per move.
        increment: Option<Duration>, 
        /// Move count until the next time control.
        moves_left: Option<usize>,
    },
}

impl TimeControl {
    pub(crate) fn allocate_time(self, book_distance: Option<usize>) -> AllocatedTime {
        match self {
            TimeControl::Infinite => AllocatedTime::Forever,
            TimeControl::MoveTime(t) => AllocatedTime::Fixed(t),
            TimeControl::TimeLeft { time_left, increment: _, moves_left } => {

                // if only one move is left, remaining is to be considered fixed allocated time
                if let Some(left) = moves_left {
                    if left == 0 { return AllocatedTime::Fixed(time_left) }
                }

                // bias towards using time sooner rather than later
                // strongly bias towards moves immediately after the opening
                let bias = if let Some(book_distance) = book_distance {
                    let begin_dist_factor = cmp::min(book_distance, BOOK_DIST) as f64;
                    let begin_factor = BOOK_BIAS - begin_dist_factor / BOOK_DIST as f64;
                    begin_factor * MOVE_BIAS
                } else {
                    MOVE_BIAS_NO_BOOK
                };
                
                // determine partition. allow for time extention/panic time
                let part = moves_left.map_or(UNKNOWN_MOVES_LEFT, |ml| ml as f64 + MOVES_LEFT_OFFSET);
                let time = time_left.mul_f64(bias).div_f64(part);

                // establish a hard cutoff time
                let cutoff = cmp::min(
                    time.mul_f64(MAX_TIME_RATIO),
                    time_left.mul_f64(MAX_TIME_LEFT_RATIO)
                );
                
                AllocatedTime::Fancy { time, cutoff }
            }
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocatedTime {
    /// Search should never be automatically halted.
    Forever,

    /// Search should use all time available.
    Fixed(Duration),

    /// Search should try to minimize time spent while maximising strength.
    /// 
    /// * `time` is the allocated search duration, as a heuristic.
    /// * `cutoff` is the total allowable search time, as a maximum.
    Fancy { time: Duration, cutoff: Duration },
}


/// Maximum search time fraction after which time is cut off or extended.
const MAX_SEARCH: f64 = 0.5;
/// Search time cutoff for re-assess as ratio of allocated time.
const SEARCH_OVERSHOOT: f64 = 1.25;
/// Minimum multiple of allocated time to allocate as panic time.
const MIN_PANIC_EXT: f64 = 1.75;
/// Minimum drop for panic allocation.
const PANIC_MIN_THRESH: i16 = 150;
/// Maximum drop for full panic allocation.
const PANIC_MAX_THRESH: i16 = 350;

///// Minimum search time fraction before time-cutoffs are considered.
//const MIN_SEARCH: f64 = 0.125;
///// Difference between PV and second-best move lending itself to decisiveness.
//const DECISIVE_PV_DIFF: i16 = 200;

pub struct TimeManager {
    //recapture: Option<Sq>,
    allocated_time: Duration,
    maximum_time: Duration,

    search_begin: Instant,
    iter_begin: Instant,


    prev_iter: Option<Duration>,
    avrg_ratio: Option<f64>,
    iter_count: usize,

    avrg_score: [Option<i16>; 2],

    prev_pv: Option<Move>,
    pv_concurr_count: usize,
}

impl TimeManager {
    pub fn start(allocated_time: Duration, maximum_time: Duration) -> Self {
        Self {
            allocated_time,
            maximum_time,

            search_begin: Instant::now(), 
            iter_begin: Instant::now(),

            iter_count: 0, 
            prev_iter: None, 
            avrg_ratio: None, 
            avrg_score: [None, None],
            prev_pv: None,
            pv_concurr_count: 0,
        }
    }

    /// Update the time manager, and check in on search status.
    /// 
    /// Returns `None` to indicate search termination, else returns the
    /// target cutoff search duration (kill the search if it is reached)
    /// which is always equal or larger than it was previously. 
    /// 
    /// `info` is expected to have a nonzero `pv` and `Some(_) = eval`.
    pub fn update(&mut self, info: &SearchInfo) -> Option<Duration> {
        assert_ne!(info.pv.len(), 0);
        assert!(info.eval.is_some());

        let evaluation;
        match info.eval.unwrap() {
            // If a forced mate is found, play immediately
            SearchEval::Mate(_) => return None,
            SearchEval::Normal(eval) => evaluation = eval,
        }
        
        // update time data
        let t = Instant::now();
        let iter_time = t - self.iter_begin;
        let search_time = t - self.search_begin;

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
        let pv = info.pv[0];
        let is_new_pv = self.prev_pv.is_none() || self.prev_pv.unwrap() == pv;
        self.prev_pv = Some(pv);
        self.pv_concurr_count += 1;
        if is_new_pv { self.pv_concurr_count = 0; }

        // update score data
        let avrg_score_ref = &mut self.avrg_score[self.iter_count % 2];
        if let Some(avrg_score) = *avrg_score_ref {
            let half_iter_count = self.iter_count as i16 / 2;
            let avrg = (avrg_score * half_iter_count + evaluation) / (half_iter_count + 1);
            *avrg_score_ref = Some(avrg);
        } else {
            *avrg_score_ref = Some(evaluation);
        }

        // update time
        self.iter_begin = Instant::now();


        if search_time > self.allocated_time.mul_f64(MAX_SEARCH) {
            // determine if panic/extra time is necessary, else return
            return if let Some(avrg_score) = avrg_score_ref.map(|s| s) {
                if evaluation < avrg_score - PANIC_MIN_THRESH {
                    // allocate panic time
                    // panic time is allocated as an adjustment of 
                    let min_panic_time = self.allocated_time.mul_f64(MIN_PANIC_EXT);
                    if min_panic_time >= self.maximum_time {
                        // allocate all
                        Some(self.maximum_time)
                    } else {
                        // allocate proportion
                        let eval_drop = (evaluation - PANIC_MIN_THRESH).max(-PANIC_MAX_THRESH);
                        let panic_lerp = eval_drop as f64 / (PANIC_MAX_THRESH - PANIC_MIN_THRESH) as f64;
                        let time = (self.maximum_time - min_panic_time).mul_f64(panic_lerp) + min_panic_time;
                        Some(time)
                    }
                } else {
                    None
                }
            } else {
                // no score yet established; likely search explosion? allocate all time
                Some(self.maximum_time)
            }
        }
        
        // todo: reimplement this using refutation speed?
        /* // check if elapsed is a significant fraction of allocated
        else if search_time > self.allocated_time.mul_f64(MIN_SEARCH) {
            // handle the case of an obvious recapture
            if self.recapture.map_or(false, |sq| sq == pv.to) {
                if pv_diff > DECISIVE_PV_DIFF { return None; }
            }
        } */

        Some(self.allocated_time.mul_f64(SEARCH_OVERSHOOT))
    }
}
