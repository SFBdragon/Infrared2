mod uci;
mod tests;

fn main() {
    // handle/config args
    for arg in std::env::args() {
        match arg.to_lowercase().as_str() {
            "testpos" => { tests::test_pos(); return; },
            "lctii" => { tests::lct_ii_test(); return; },
            "brantko" => { tests::brantko_kopec_test(); return; },
            "eigmann" => { tests::eigmann_rapid_test(); return; },
            "silent" => { tests::silent_but_deadly_test(); return; },
            _ => (),
        }
    }
    
    // Run UCI Engine!
    uci::uci();
}
