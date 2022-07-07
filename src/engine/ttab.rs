use std::mem::size_of;
use parking_lot::Mutex;


#[derive(Debug, Clone)]
pub struct Bucket<T> {
    hash: u64,
    val: T,
}

#[derive(Debug)]
pub struct HashTable<T> {
    data: Box<[Mutex<Option<Bucket<T>>>]>,
}

impl<T> HashTable<T> {
    /// Returns a `HashTable` occupying up to the number of bytes specified.
    pub fn with_memory(bytes: usize) -> Self {
        let capacity = bytes / size_of::<Mutex<Option<Bucket<T>>>>();
        Self::with_capacity(capacity)
    }

    /// Returns a `HashTable` with the specified capacity to the previous prime.
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
        for _ in 0..capacity { data.push(Mutex::new(None)); }
        
        Self {
            data: data.into_boxed_slice()
        }
    }

    /// Insert `val` into the table given `hash`.
    /// 
    /// If `hash` already is present, the value is updated to `val`.
    /// If a collision occurs, `replace_strat` is invoked with the new
    /// and old value respectively, where the result determines if a replacement
    /// should occur or not.
    pub fn insert<F>(&self, hash: u64, val: T, replace_strat: F)
    where F: FnOnce(&T, &T) -> bool {
        let index = (hash % self.data.len() as u64) as usize;
        // SAFETY: index is always less than length
        let mut bucket_option = unsafe { self.data.get_unchecked(index) }.lock();

        match bucket_option.as_mut() {
            // value already exists
            Some(bucket) => {
                if bucket.hash == hash {
                    // update when hashes are the same
                    bucket.val = val;
                } else if replace_strat(&val, &bucket.val) {
                    // replace when hashes are different
                    // if the replacement strategy says to do so
                    *bucket = Bucket { hash, val };
                }
            },
            // no existing value, simply insert
            None => *bucket_option = Some(Bucket { hash, val }),
        }
    }

    /* /// Lock the bucket and retrieve the contained value.
    pub fn get(&self, hash: u64) -> Option<MappedMutexGuard<T>> {
        let index = (hash % self.data.len() as u64) as usize;
        let guard = self.data[index].lock();

        MutexGuard::try_map(guard, |bo| bo.as_mut().and_then(|b|
            if b.hash == hash { Some(&mut b.val) } else { None }
        )).ok()
    } */
}
impl<T: Clone> HashTable<T> {
    /// Lock the bucket and retrieve the contained value.
    pub fn get(&self, hash: u64) -> Option<T> {
        let index = (hash % self.data.len() as u64) as usize;
        self.data[index]
            .lock()
            .as_ref()
            .and_then(|b| (b.hash == hash).then_some(b.val.clone()))
    }
}


/// Minimax with Alpha-Beta score type.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScoreKind {
    /// The full minimax score of the given node.
    Exact,
    /// A lower-bound score returned by a Cut-Node that failed high (score >= beta).
    LoBound,
    /// An upper-bound score returned by an All-Node that failed low (score <= alpha).
    HiBound,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SearchNode {
    /// Minimax score.
    pub score: i16,
    /// Alpha-Beta score kind.
    pub kind: ScoreKind,

    /// Ply distance to typical search depth.
    pub draft: i8,
    /// Principal variation move.
    pub pv: Option<crate::Move>,
}

pub type TransTable = HashTable<SearchNode>;


#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use super::*;

    /* #[test]
    fn test_transposition_table() {
        let tt = HashTable::with_capacity(10000);
        tt.insert(0xa09b67a, 6usize, |_, _| panic!());
        assert_eq!(*tt.get(0xa09b67a).unwrap().deref(), 6);
        
        tt.insert(0xa09b6a, 2usize, |_, _| panic!());
        assert_eq!(*tt.get(0xa09b6a).unwrap().deref(), 2);

        tt.insert(0xa09b67a, 7usize, |_, _| panic!());
        assert_eq!(*tt.get(0xa09b67a).unwrap().deref(), 7);

        tt.insert(0xa09b67a, 8usize, |_, _| panic!());
        assert_eq!(*tt.get(0xa09b67a).unwrap().deref(), 8);
    } */
}

