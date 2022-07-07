pub mod board;
pub mod engine;

pub use board::{Move, Board, Piece, GameOver};
pub use board::zobrist::{PosHashMap, U64IdentHashBuilder};


/// Evaluates a boolean expression:
/// * Where `true` returns `Ok(())`
/// * Where `false` returns `Err(&'static str)` describing the failure.
/// 
/// In practice, it's useful as a recoverable `assert!` alternative.
#[macro_export]
macro_rules! as_result {
    ($cond:expr) => {
        bool::then_some($cond, ()).ok_or(
            concat!(
                "assertion failed: `",
                stringify!($cond),
                "`"
            )
        )
    };
}

/// Loop through the index of each set bit from least to most significant.
#[macro_export]
macro_rules! for_sq {
    ($sq:ident in $bb:expr => $blk:expr) => {
        let mut t = $bb;
        while t != 0 {
            let $sq = t.trailing_zeros() as u8;
            { $blk }
            t &= t - 1;
        }
    };
}


