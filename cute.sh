cutechess-cli -debug -pgnout tournament.pgn \
-engine name=infra2 proto=uci cmd=~/src/rust/infra/target/release/infra dir=~/src/rust/infra/target/release/ \
-engine name=pigeon proto=uci cmd=~/Downloads/pigeon-1.5.1/pigeon-1.5.1 dir=~/Downloads/pigeon-1.5.1/ \
-each tc=40/5:00