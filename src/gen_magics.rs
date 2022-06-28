
fn bishop_shadow(sq: u8) -> u64 {
    let mut msk = 0u64;
    let file = sq % 8;
    let rank = sq / 8;
    for j in 1..std::cmp::min(rank, file) {
        msk |= 1 << (file - j + (rank - j) * 8);
    }
    for j in 1..std::cmp::min(7-rank, 7-file) {
        msk |= 1 << (file + j + (rank + j) * 8);
    }
    for j in 1..std::cmp::min(rank, 7-file) {
        msk |= 1 << (file + j + (rank - j) * 8);
    }
    for j in 1..std::cmp::min(7-rank, file) {
        msk |= 1 << (file - j + (rank + j) * 8);
    }
    msk
}
fn bishop_fend(sq: u8, all: u64) -> u64 {
    let mut mov_msk = 0u64;
    let file = sq % 8;
    let rank = sq / 8;
    
    for j in 1..=std::cmp::min(rank, file) {
        mov_msk |= 1 << (file - j + (rank - j) * 8);
        if 1 << (file - j + (rank - j) * 8) & all != 0 { break; }
    }
    for j in 1..=std::cmp::min(7-rank, 7-file) {
        mov_msk |= 1 << (file + j + (rank + j) * 8);
        if 1 << (file + j + (rank + j) * 8) & all != 0 { break; }
    }
    for j in 1..=std::cmp::min(rank, 7-file) {
        mov_msk |= 1 << (file + j + (rank - j) * 8);
        if 1 << (file + j + (rank - j) * 8) & all != 0 { break; }
    }
    for j in 1..=std::cmp::min(7-rank, file) {
        mov_msk |= 1 << (file - j + (rank + j) * 8);
        if 1 << (file - j + (rank + j) * 8) & all != 0 { break; }
    }

    mov_msk
}

fn rook_shadow(sq: u8) -> u64 {
    let mut msk: u64 = 0;
    let file = sq % 8;
    let rank = sq / 8;

    msk |= 0x7e << rank * 8;
    msk |= 0x0001010101010100 << file;
    msk &= !(1 << sq);
    msk
}
fn rook_fend(sq: u8, all: u64) -> u64 {
    let mut mov_msk = 0u64;
    let file = sq % 8;
    let rank = sq / 8;
    
    for j in 1..=file {
        mov_msk |= 1 << (file - j + rank * 8);
        if 1 << (file - j + rank * 8) & all != 0 { break; }
    }
    for j in 1..=(7-file) {
        mov_msk |= 1 << (file + j + rank * 8);
        if 1 << (file + j + rank * 8) & all != 0 { break; }
    }
    for j in 1..=rank {
        mov_msk |= 1 << (file + (rank - j) * 8);
        if 1 << (file + (rank - j) * 8) & all != 0 { break; }
    }
    for j in 1..=(7-rank) {
        mov_msk |= 1 << (file + (rank + j) * 8);
        if 1 << (file + (rank + j) * 8) & all != 0 { break; }
    }

    mov_msk
}



#[allow(unused)]
pub fn gen_magics(rook_else_bishop: bool) {
    // (offset, mask, magic, shift)
    let mut magic_data = Vec::<(usize, u64, u64, u32)>::with_capacity(64);
    let mut fends_table = Vec::<u64>::new();
    let rng = fastrand::Rng::new();

    for sq in 0..64 {
        let shadow = if rook_else_bishop { rook_shadow(sq) } else { bishop_shadow(sq) };
        let bits = shadow.count_ones();
        let n = 1 << bits;
        let mut fends = Vec::with_capacity(n);

        for i in 0..n {
            let all = unsafe { std::arch::x86_64::_pdep_u64(i as u64, shadow) };
            let fend = if rook_else_bishop { rook_fend(sq, all) } else { bishop_fend(sq, all) };
            fends.push((all, fend));
        }

        let mut used = vec![0; n];
        let magic = 'magic: loop {
            let magic = rng.u64(..) & rng.u64(..) & rng.u64(..);
            if (magic & 0xff00000000000000).count_ones() < 6 { continue; }

            used.fill(0);
            for &(all, fend) in fends.iter() {
                let idx = (all.wrapping_mul(magic) >> 64 - bits) as usize;
                if used[idx] == 0 { used[idx] = fend; }
                else if used[idx] != fend { continue 'magic; }
            }

            println!("found magic {sq}");
            break magic;
        };

        let offset = fends_table.len();
        let shift = 64 - bits;
        magic_data.push((offset, shadow, magic, shift));
        for _ in 0..n { fends_table.push(0); }
        for (all, fend) in fends { fends_table[offset + (all.wrapping_mul(magic) as usize >> shift)] = fend; }
    }

    let mut dat_file = std::fs::File::create(std::path::Path::new(
        if rook_else_bishop { "src/board/rook.dat" } else { "src/board/bishop.dat" }
    )).unwrap();
    use std::io::Write;
    write!(dat_file, "{:?}", &fends_table[..]).unwrap();

    println!("{:#x?}", magic_data);
}

