# Infrared Chess Engine v2

A UCI-compatible open source chess engine. Written in Rust by Shaun Beautement. 

### Playing Strength
2193 +/−30 ELO on CCRL after 400 games, i.e. almost master level, although CCRL and FIDE ratings aren't directly comparable.

### Usage
1. Install a Chess GUI such as [Cute Chess](https://cutechess.com/) (Windows installer can be downloaded [here](https://github.com/cutechess/cutechess/releases/download/1.2.0/cutechess_setup.exe)).
2. Download and unzip the appropriate version for your operating system under [Releases](https://github.com/SFBdragon/Infrared2/releases).
3. Configure the engine (for Cute Chess, navigate to Tools -> Settings -> Engines (tab) -> + -> locate engine executable).
4. Play against it! (for Cute Chess, Game -> New -> set a player to CPU -> select your configuration for Infrared 2).

### Platforms
Builds are available for Linux and Windows 64-bit under [Releases](https://github.com/SFBdragon/Infrared2/releases), but other targets can be compiled for by downloading the source, installing a Rust toolchain for your system, e.g. with [Rustup](https://www.rust-lang.org/tools/install), and then invoking `cargo run --release` in the directory of the dowload of this repository. 

(If this is too confusing, please email me!)

### Configuration
The engine is by default configured to:
- Use its own book (polyglot-like format)
- Query online 7-man Syzygy tables
- Run as parallel as there are available hardware threads (up to 16)
- Employ 256MiB of memory for its hash table

These can all be configured/disabled via UCI configuration (or deleting the opening book).
