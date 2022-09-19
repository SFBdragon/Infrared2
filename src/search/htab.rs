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

    /// Returns a `Self` with the specified capacity to the previous power of two.
    pub fn with_capacity(mut capacity: usize) -> Self {
        capacity = (capacity / 2).next_power_of_two();

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

        let index = hash & self.index_mask;
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
        let index = hash & self.index_mask;
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
