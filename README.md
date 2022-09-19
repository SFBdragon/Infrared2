# Infrared Chess Engine v2

A UCI-compatible open source chess engine. Written in Rust by Shaun Beautement. 

### Playing Strength
TBD (maybe ~2400 ELO on CCRL?)

### Usage
1. Install a Chess GUI such as [Cute Chess](https://cutechess.com/).
2. Download and unzip the appropriate version for your operating system under [Releases](https://github.com/SFBdragon/Infrared2/releases).
3. Configure the engine (for Cute Chess, navigate to Tools -> Settings -> Engines (tab) -> + -> locate engine executable).
4. Play against it! (for Cute Chess, Game -> New -> set a player to CPU -> select your configuration for Infrared 2).

### Platforms
Builds are available for Linux and Windows 64-bit under [Releases](https://github.com/SFBdragon/Infrared2/releases), but other targets should be easily compiled for by downloading the source, setting up a rust toolchain for your machine and invoking `cargo build --release`, and optionally including the `openings.dat` file alongside the executable.

### Configuration
The engine is by default configured to:
- Use its own book (polyglot-like format)
- Query online 7-man Syzygy tables
- Run as parallel as there are available hardware threads (up to 16)
- Employ 256MiB of memory for its hash table

These can all be configured/disabled via UCI configuration (or deleting the opening book).
