mod uci;
mod tests;

fn main() {
    // handle/config args
    if let Some(arg) = std::env::args().nth(1) {
        if arg == "epd" {
            if let Some(path) = std::env::args().nth(2) {
                let path = std::path::Path::new(path.as_str());
                assert!(path.exists(), "File at this path does not exist!");
                if let Some(secs) = std::env::args().nth(3) {
                    let secs = secs.parse::<f32>().expect("Invalid time in seconds");
                    let time = std::time::Duration::from_secs_f32(secs);
                    tests::epd_test_from_file(path, time);
                    return;
                }
            }
            println!("Invalid epd command.");
            println!("infra[EXE] epd <filepath> <seconds per search>");
        } else if arg == "test" {
            tests::test_pos();
        } else {
            println!("Unknown arg(s)!");
            println!("Available args: ");
            println!("infra[EXE] test");
            println!("infra[EXE] epd <filepath> <seconds per search>");
        }
    } else {
        // Run UCI Engine!
        uci::uci();
    }
}
