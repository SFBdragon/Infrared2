//! Time control logic.



pub enum TimeControl {
    Infinite,
    SuddenDeath(usize, usize),
    Increment(usize, usize, usize),
}

pub fn max_search_duration(_tc: TimeControl) -> f64 {
    todo!()
}

pub struct IterTimeData {
    /// Previous search iteration duration in seconds.
    pub prev_time: f64,
    pub avg_ratio: f64,
}
