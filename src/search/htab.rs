//! Shared hash table implementation for Transposition and Pawn-King-Evaluation tables.

use std::{mem::size_of, cell::UnsafeCell, marker::PhantomData};
use crate::{Move, Sq, Piece, KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN};


#[derive(Debug, Default)]
struct Bucket {
    key_data: UnsafeCell<u64>,
    data: UnsafeCell<u64>,
}
impl Bucket {
    #[inline]
    fn read(&self, key: u64) -> Option<Result<u64, u64>> {
        unsafe {
            // https://www.chessprogramming.org/Shared_Hash_Table#Xor
            // quadword reads and writes are atomic on modern processors
            // xor confirms that the key, key^data, and data match
            let kd = *self.key_data.get();
            let data = *self.data.get();

            if data == 0 {
                None
            } else {
                Some((kd ^ key == data).then_some(data).ok_or(data))
            }
        }
    }

    #[inline]
    fn write(&self, key: u64, data: u64) {
        unsafe {
            *self.key_data.get() = key ^ data;
            *self.data.get() = data;
        }
    }
}


/// Implementation requirement: `0u64` is not a valid variant.
pub trait HashTableData {
    fn from_u64(data: u64) -> Self;
    fn to_u64(self) -> u64;
}


/// Lock-less, concurrent, parallel, simple hash table implementation.
pub struct HashTable<T: HashTableData> { 
    data: Box<[Bucket]>,
    index_mask: u64,
    _phantom: PhantomData<T>,
}

unsafe impl<T: HashTableData> Send for HashTable<T> {}
unsafe impl<T: HashTableData> Sync for HashTable<T> {}

impl<T: HashTableData> HashTable<T> {
    /// Returns a `Self` occupying up to the number of bytes specified.
    pub fn with_memory(bytes: usize) -> Self {
        let capacity = bytes / size_of::<Bucket>();
        Self::with_capacity(capacity)
    }

    /// Returns a `Self` with the specified capacity to the previous prime.
    pub fn with_capacity(mut capacity: usize) -> Self {
        /* fn prev_prime(mut n: usize) -> usize {
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
        } */

        capacity = (capacity / 2).next_power_of_two(); /* prev_prime(capacity); */

        let mut data = Vec::with_capacity(capacity);
        for _ in 0..capacity { 
            data.push(Bucket::default()); 
        }
        
        Self {
            data: data.into_boxed_slice(),
            index_mask: (capacity - 1) as u64,
            _phantom: PhantomData,
        }
    }

    /// Insert `val` into the table given `hash`.
    /// 
    /// If `hash` is already present, the value is updated to `val`.
    /// If a collision occurs, `replace_strat` is invoked with the
    /// old value, and the return value determines if a replacement
    /// should occur or not.
    #[inline]
    pub fn insert<F>(&self, hash: u64, node: T, replace_strat: F)
    where F: FnOnce(&T) -> bool {

        let index = hash & self.index_mask; /* (hash % self.data.len() as u64) as usize; */
        // SAFETY: index is always less than length
        let bucket = unsafe { self.data.get_unchecked(index as usize) };

        let read = bucket.read(hash);

        if read.is_none() 
        || read.unwrap().is_ok() 
        || replace_strat(&T::from_u64(read.unwrap().unwrap_err())) {
            bucket.write(hash, node.to_u64());
        }
    }

    
    /// Lock the bucket and retrieve the contained value.
    #[inline]
    pub fn get(&self, hash: u64) -> Option<T> {
        let index = hash & self.index_mask; /* (hash % self.data.len() as u64) as usize; */
        // SAFETY: index is always less than length
        let bucket = unsafe { self.data.get_unchecked(index as usize) };

        bucket.read(hash)?.map_or(None, |d| Some(T::from_u64(d)))
    }
}



// const TT_DISC_NONE: u8 = 0;
const TT_DISC_EXACT: u8 = 1;
const TT_DISC_LO_BOUND: u8 = 2;
const TT_DISC_HI_BOUND: u8 = 3;
const TT_DISC_CHECKMATE: u8 = 4;
const TT_DISC_STALEMATE: u8 = 5;

const TT_DISCRIMINANT: usize = 0;
const TT_DRAFT: usize = 1;
const TT_SCORE_LO: usize = 2;
const TT_SCORE_HI: usize = 3;
const TT_PV_FROM: usize = 4;
const TT_PV_TO: usize = 5;
const TT_PV_PIECE: usize = 6;

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


/// Alpha-Beta search result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchNode {
    Score {
        /// Alpha-Beta score.
        score: i16,
        /// Alpha-Beta score kind.
        score_kind: ScoreKind,
        /// Ply distance to horizon.
        draft: i8,
        /// Best move.
        pv: Move,
    },
    Checkmate,
    Stalemate,
}


impl HashTableData for SearchNode {
    #[inline]
    fn from_u64(data: u64) -> Self {
        let bytes = data.to_le_bytes();

        let score_kind;
        match bytes[TT_DISCRIMINANT] {
            TT_DISC_EXACT => score_kind = ScoreKind::Exact,
            TT_DISC_LO_BOUND => score_kind = ScoreKind::LoBound,
            TT_DISC_HI_BOUND => score_kind = ScoreKind::HiBound,
            TT_DISC_CHECKMATE => return SearchNode::Checkmate,
            TT_DISC_STALEMATE => return SearchNode::Stalemate,
            _ => unreachable!(),
        }

        SearchNode::Score {
            score: i16::from_le_bytes([bytes[TT_SCORE_LO], bytes[TT_SCORE_HI]]),
            score_kind,
            draft: bytes[TT_DRAFT] as i8,
            pv: Move::new(
                Sq::new(bytes[TT_PV_FROM]), 
                Sq::new(bytes[TT_PV_TO]), 
                match bytes[TT_PV_PIECE] {
                    KING => Piece::King,
                    QUEEN => Piece::Queen,
                    ROOK => Piece::Rook,
                    BISHOP => Piece::Bishop,
                    KNIGHT => Piece::Knight,
                    PAWN => Piece::Pawn,
                    _ => unreachable!(),
                }
            ),
        }
    }

    #[inline]
    fn to_u64(self) -> u64 {
        let mut bytes = [0u8; 8];

        match self {
            SearchNode::Checkmate => bytes[TT_DISCRIMINANT] = TT_DISC_CHECKMATE,
            SearchNode::Stalemate => bytes[TT_DISCRIMINANT] = TT_DISC_STALEMATE,
            SearchNode::Score { score, score_kind, draft, pv } => {
                match score_kind {
                    ScoreKind::Exact => bytes[TT_DISCRIMINANT] = TT_DISC_EXACT,
                    ScoreKind::LoBound => bytes[TT_DISCRIMINANT] = TT_DISC_LO_BOUND,
                    ScoreKind::HiBound => bytes[TT_DISCRIMINANT] = TT_DISC_HI_BOUND,
                }
                bytes[TT_SCORE_LO] = score.to_le_bytes()[0];
                bytes[TT_SCORE_HI] = score.to_le_bytes()[1];

                bytes[TT_DRAFT] = draft as u8;

                bytes[TT_PV_FROM] = pv.from.u8();
                bytes[TT_PV_TO] = pv.to.u8();
                bytes[TT_PV_PIECE] = pv.piece as u8;
            },
        }

        u64::from_le_bytes(bytes)
    }
}

pub type TransTable = HashTable<SearchNode>;
pub const TRANS_MEM_DEFAULT: usize = 1024 * 1024 * 256;

pub struct PawnKingEval {
    pub eval: f32,
}

impl HashTableData for PawnKingEval {
    fn from_u64(data: u64) -> Self {
        Self {
            eval: f32::from_bits(data as u32)
        }
    }

    fn to_u64(self) -> u64 {
        self.eval.to_bits() as u64
    }
}

pub type PkEvalTable = HashTable<PawnKingEval>;
pub const PK_EVAL_MEM_DEFAULT: usize = 1024 * 1024 * 4;

/* impl SearchNode {
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

        SearchNode::Score {
            score_kind,
            data: data.assume_init(),
        }
    }

    fn to_raw_parts(self) -> (u8, MaybeUninit<TtData>) {
        match self {
            SearchNode::Score { score_kind, data } => {
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
} */


/* #[cfg(test)]
mod tests {
    use crate::{Sq, Move, Piece};

    use super::*;

    #[test]
    fn test_transposition_table() {
        let tt = HashTable::with_capacity(1000);
        tt.insert(0xa09b67a, SearchNode::Checkmate, |_| panic!());
        assert_eq!(tt.get(0xa09b67a).unwrap(), SearchNode::Checkmate);
        
        tt.insert(0xa09b6a, SearchNode::Stalemate, |_| panic!());
        assert_eq!(tt.get(0xa09b6a).unwrap(), SearchNode::Stalemate);

        assert!(tt.get(0x234).is_none());

        tt.insert(0xa09b67a, SearchNode::Score {
            score_kind: ScoreKind::HiBound, data: TtData { score: 1, draft: 3, pv: Move::new(Sq::A1, Sq::A2, Piece::Pawn) } 
        }, |_| panic!());
        assert_eq!(tt.get(0xa09b67a).unwrap(), SearchNode::Score {
            score_kind: ScoreKind::HiBound, data: TtData { score: 1, draft: 3, pv: Move::new(Sq::A1, Sq::A2, Piece::Pawn) } 
        });
    }
} */


/* 

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
} */


    /* /// Insert `val` into the table given `hash`.
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
    } */


