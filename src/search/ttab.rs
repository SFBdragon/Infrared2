use std::{mem::{size_of, MaybeUninit}, cell::UnsafeCell, sync::atomic::{AtomicBool, Ordering}};

/// Transposition Table.
pub struct TransTable {
    data: Box<[TtBucket]>,
}

unsafe impl Send for TransTable {}
unsafe impl Sync for TransTable {}

impl Default for TransTable {
    fn default() -> Self {
        const DEFAULT_MEMORY: usize = 1024 * 1024 * 256;
        
        Self::with_memory(DEFAULT_MEMORY)
    }
}

impl TransTable {
    /// Returns a `Self` occupying up to the number of bytes specified.
    pub fn with_memory(bytes: usize) -> Self {
        let capacity = bytes / size_of::<TtBucket>();
        Self::with_capacity(capacity)
    }

    /// Returns a `Self` with the specified capacity to the previous prime.
    pub fn with_capacity(mut capacity: usize) -> Self {
        fn prev_prime(mut n: usize) -> usize {
            if n & 1 == 0 { n -= 1; }
        
            'n: loop {
                for fact in (2..).map(|x| x * 2 - 1).take_while(|&x| x * x <= n) {
                    if n - (n / fact * fact) == 0 {
                        n -= 2;
                        continue 'n;
                    }
                }
        
                return n;
            }
        }

        capacity = prev_prime(capacity);

        let mut data = Vec::with_capacity(capacity);
        for _ in 0..capacity { 
            data.push(TtBucket::new()); 
        }
        
        Self {
            data: data.into_boxed_slice()
        }
    }

    /// Insert `val` into the table given `hash`.
    /// 
    /// If `hash` is already present, the value is updated to `val`.
    /// If a collision occurs, `replace_strat` is invoked with the
    /// old value, and the return value determines if a replacement
    /// should occur or not.
    #[inline]
    pub fn insert<F>(&self, hash: u64, node: SearchNode, replace_strat: F)
    where F: FnOnce(&SearchNode) -> bool {
        use std::sync::atomic::Ordering::{Acquire, Relaxed, Release};

        let index = (hash % self.data.len() as u64) as usize;
        // SAFETY: index is always less than length
        let bucket = unsafe { self.data.get_unchecked(index) };

        // acquire lock (based on 'spin' crate implementation)
        while bucket.lock.compare_exchange_weak(false, true, Acquire, Relaxed).is_err() {
            while bucket.lock.load(Ordering::Relaxed) {
                std::hint::spin_loop();
            }
        }

        // critical section:
        // SAFETY: lock has been acquired, access is exclusive
        unsafe {
            let disc = *bucket.disc.get();
            let data = *bucket.data.get();
            if disc == TT_DISC_NONE || *bucket.hash.get() == hash 
            || replace_strat(&SearchNode::from_raw_parts(disc, data)) {

                let (node_disc, node_data) = node.to_raw_parts();
                *bucket.hash.get() = hash;
                *bucket.disc.get() = node_disc;
                *bucket.data.get() = node_data;
            }
        }
        // end of critical section
        bucket.lock.store(false, Release);
    }

    
    /// Lock the bucket and retrieve the contained value.
    #[inline]
    pub fn get(&self, hash: u64) -> Option<SearchNode> {
        use std::sync::atomic::Ordering::{Acquire, Relaxed, Release};

        let index = (hash % self.data.len() as u64) as usize;
        let bucket = unsafe { self.data.get_unchecked(index) };

        // acquire lock (based on 'spin' crate implementation)
        while bucket.lock.compare_exchange_weak(false, true, Acquire, Relaxed).is_err() {
            while bucket.lock.load(Relaxed) {
                std::hint::spin_loop();
            }
        }

        let value;

        // critical section:
        // SAFETY: lock has been acquired, access is exclusive
        unsafe {
            let disc = *bucket.disc.get();
            if disc != TT_DISC_NONE && *bucket.hash.get() == hash {
                value = Some(SearchNode::from_raw_parts(disc, *bucket.data.get()));
            } else {
                value = None;
            }
        }
        // end of critical section
        bucket.lock.store(false, Release);

        value
    }
}


/// Alpha-Beta score type.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScoreKind {
    /// The full minimax score of the given node.
    Exact = TT_DISC_EXACT,
    /// A lower-bound score returned by a Cut-Node that failed high (score >= beta).
    LoBound = TT_DISC_LO_BOUND,
    /// An upper-bound score returned by an All-Node that failed low (score <= alpha).
    HiBound = TT_DISC_HI_BOUND,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TtData {
    /// Negamax score.
    pub score: i16,
    /// Ply distance to horizon.
    pub draft: i8,
    /// Best move.
    pub pv: crate::Move,
}

/// Alpha-Beta search result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchNode {
    Normal {
        score_kind: ScoreKind,
        data: TtData,
    },
    Checkmate,
    Stalemate,
}


impl SearchNode {
    /// Construct `Self` from `TtBucket` components.
    /// ### Safety:
    /// Caller must guarantee that `disc` and `data` complement each others' states.
    /// 
    /// This means, in effect, that `data` must be initialized if `disc` is 
    /// `TT_DISC_EXACT`, `TT_DISC_LO_BOUND`, or `TT_DISC_HI_BOUND`.
    unsafe fn from_raw_parts(disc: u8, data: MaybeUninit<TtData>) -> Self {
        assert!(disc != 0 && disc < 6);

        let score_kind;
        match disc {
            TT_DISC_EXACT => score_kind = ScoreKind::Exact,
            TT_DISC_LO_BOUND => score_kind = ScoreKind::LoBound,
            TT_DISC_HI_BOUND => score_kind = ScoreKind::HiBound,
            TT_DISC_CHECKMATE => return SearchNode::Checkmate,
            TT_DISC_STALEMATE => return SearchNode::Stalemate,
            _ => unreachable!(),
        }

        SearchNode::Normal {
            score_kind,
            data: data.assume_init(),
        }
    }

    fn to_raw_parts(self) -> (u8, MaybeUninit<TtData>) {
        match self {
            SearchNode::Normal { score_kind, data } => {
                let disc = match score_kind {
                    ScoreKind::Exact => TT_DISC_EXACT,
                    ScoreKind::LoBound => TT_DISC_LO_BOUND,
                    ScoreKind::HiBound => TT_DISC_HI_BOUND,
                };

                (disc, MaybeUninit::new(data))
            },
            SearchNode::Checkmate => (TT_DISC_CHECKMATE, MaybeUninit::uninit()),
            SearchNode::Stalemate => (TT_DISC_STALEMATE, MaybeUninit::uninit()),
        }
    }
}

const TT_DISC_NONE: u8 = 0;
const TT_DISC_EXACT: u8 = 1;
const TT_DISC_LO_BOUND: u8 = 2;
const TT_DISC_HI_BOUND: u8 = 3;
const TT_DISC_CHECKMATE: u8 = 4;
const TT_DISC_STALEMATE: u8 = 5;

// Why this mess? Why not be sensible and use parking_lot::Mutex or something?
// Glad you asked! Whip out your trusty `size_of::<T>()` and measure your safe
// version against this one. You probably will get an interval of 24 bytes. 
// This version is 16 bytes, which allows for 50% more buckets per unit memory.
// This is a dramatic improvement, and seen to be worth the (contained)
// increase in complexity. 

struct TtBucket {
    lock: AtomicBool,
    disc: UnsafeCell<u8>,
    data: UnsafeCell<MaybeUninit<TtData>>,
    hash: UnsafeCell<u64>,
}
impl TtBucket {
    pub fn new() -> Self {
        Self {
            lock: AtomicBool::new(false),
            disc: UnsafeCell::new(TT_DISC_NONE),
            hash: UnsafeCell::new(0),
            data: UnsafeCell::new(MaybeUninit::uninit()), 
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transposition_table() {
        let tt = TransTable::with_capacity(1000);
        tt.insert(0xa09b67a, SearchNode::Checkmate, |_| panic!());
        assert_eq!(tt.get(0xa09b67a).unwrap(), SearchNode::Checkmate);
        
        tt.insert(0xa09b6a, SearchNode::Stalemate, |_| panic!());
        assert_eq!(tt.get(0xa09b6a).unwrap(), SearchNode::Stalemate);

        assert!(tt.get(0x234).is_none());

        tt.insert(0xa09b67a, SearchNode::Normal {
            score_kind: ScoreKind::HiBound, data: TtData { score: 1, draft: 3, pv: crate::Move::new(0, 0, crate::Piece::Pawn) } 
        }, |_| panic!());
        assert_eq!(tt.get(0xa09b67a).unwrap(), SearchNode::Normal {
            score_kind: ScoreKind::HiBound, data: TtData { score: 1, draft: 3, pv: crate::Move::new(0, 0, crate::Piece::Pawn) } 
        });
    }
}

