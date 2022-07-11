use std::{fs::File, io::Write};

use infra::{Board, Move, Piece, for_mov, board::flip_sq};
use pgn_reader::{Visitor, Role, Outcome, Color};
use id_tree::*;

const PGN_FILE: &str = "D:\\temp\\twic.pgn";
const BOOK_FILE: &str = "openings.dat";

fn main() {
    let pgn_file = File::open(PGN_FILE).unwrap();
    let pgn_reader = pgn_reader::BufferedReader::new(pgn_file);

    let mut total_importance = 0;
    let mut total_positions = 0;

    let mut graph = TreeBuilder::new()
        .with_root(Node::new(PosData { hash: 0, importance: 0, mov: Move { from_sq: 64, to_sq: 64, piece: Piece::King } }))
        .build();

    let mut visitor = MyVisitor {
        parse_failed: false,
        board: Board::default(),
        data: Vec::new(),
        outcome: None,
    };

    for parse in pgn_reader.into_iter(&mut visitor)/* .take(100000) */ {
        match parse {
            Ok(parse) => {
                if let Some((hashes, outcome)) = parse {
                    // populate graph!
                    let mut colour = 1;
                    let mut node_id = graph.root_node_id().unwrap().clone();
                    for &(hash, mov) in hashes.iter() {
                        let mut found_child = false;
                        'children: for child_id in graph.children_ids(&node_id).unwrap() {
                            if graph.get(child_id).unwrap().data().mov == mov {
                                node_id = child_id.clone();
                                found_child = true;
                                break 'children;
                            }
                        }
                        if !found_child {
                            node_id = graph.insert(
                                Node::new(PosData { hash, importance: 0, mov }),
                                id_tree::InsertBehavior::UnderNode(&node_id),
                            ).unwrap();
                            total_positions += 1;
                        }

                        let node = graph.get_mut(&node_id).unwrap();
                        let importance = std::cmp::max(colour * outcome, 0) as u64;
                        node.data_mut().importance += importance;
                        total_importance += importance;

                        colour = -colour;
                    }
                } else {
                    eprintln!("parse failed!");
                }
            },
            Err(err) => eprintln!("parse failed worse: {err}"),
        }
    }

    eprintln!("all games finished!");

    let mut positions = Vec::new();
    for node in graph.traverse_level_order(graph.root_node_id().unwrap()).unwrap() {
        if node.data().importance >= 3 {
            positions.push(*node.data());
        }
    }
    positions.sort_by_key(|x| x.hash);
    
    let mut moves = Vec::<usize>::new();
    'pos: for i in 1..positions.len() {
        if positions[i].hash == positions[i-1].hash {
            for &j in moves.iter() {
                if positions[j].mov == positions[i].mov {
                    positions[i].importance += positions[j].importance;
                    positions[j].importance = 0;
                    continue 'pos;
                }
            }
            moves.push(i);
        } else {
            moves.clear();
            moves.push(i);
        }
    }

    let mut writer = 0;
    for i in 0..positions.len() {
        if positions[i].importance != 0 {
            positions[writer] = positions[i];
            writer += 1;
        }
    }
    positions.truncate(writer);

    let mut book = File::create(BOOK_FILE).unwrap();
    for pos in positions {
        book.write(&pos.hash.to_le_bytes()).unwrap();
        book.write(&(pos.importance as u32).to_le_bytes()).unwrap();
        book.write(&pos.mov.from_sq.to_le_bytes()).unwrap();
        book.write(&pos.mov.to_sq.to_le_bytes()).unwrap();
        book.write(&(pos.mov.piece as u8).to_le_bytes()).unwrap();
        book.write(&[0xFF]).unwrap();
    }


    println!("importance: {total_importance}, positions: {total_positions}");
}

#[derive(Debug, Clone, Copy)]
pub struct PosData {
    pub hash: u64,
    pub mov: Move,
    pub importance: u64,
}

pub struct MyVisitor {
    pub parse_failed: bool,
    pub board: Board,
    pub data: Vec<(u64, Move)>,

    pub outcome: Option<Outcome>,
}

impl Visitor for MyVisitor {
    type Result = Option<(Vec<(u64, Move)>, i8)>;

    fn begin_game(&mut self) {
        self.parse_failed = false;
        self.board = Board::default();
        self.data.clear();
        self.outcome = None;
    }

    fn begin_headers(&mut self) {}

    fn header(&mut self, key: &[u8], value: pgn_reader::RawHeader<'_>) {
        match key {
            b"Result" => match value.as_bytes() {
                b"1-0" => self.outcome = Some(Outcome::Decisive { winner: pgn_reader::Color::White }),
                b"0-1" => self.outcome = Some(Outcome::Decisive { winner: pgn_reader::Color::Black }),
                b"1/2-1/2" => self.outcome = Some(Outcome::Draw),
                _ => (),
            },
            _ => (),
        }
    }

    fn end_headers(&mut self) -> pgn_reader::Skip {
        pgn_reader::Skip(/* false */if let Some(Outcome::Draw) = self.outcome { true } else { false })
    }

    fn san(&mut self, san_plus: pgn_reader::SanPlus) {
        if self.board.move_count > 10 || self.parse_failed { return; }

        if let Some(mov) = parse_san(&self.board, san_plus) {
            self.data.push((self.board.hash, mov));
            self.board.make(mov);
        } else {
            self.parse_failed = true;
        }
    }

    fn nag(&mut self, _nag: pgn_reader::Nag) {}
    fn comment(&mut self, _comment: pgn_reader::RawComment<'_>) {}
    fn begin_variation(&mut self) -> pgn_reader::Skip { pgn_reader::Skip(true) }
    fn end_variation(&mut self) {}
    
    fn end_game(&mut self) -> Self::Result {
        match self.parse_failed {
            false => Some((std::mem::replace(&mut self.data, Vec::new()), match self.outcome {
                Some(Outcome::Decisive { winner: Color::White }) => 1,
                Some(Outcome::Decisive { winner: Color::Black }) => -1,
                Some(Outcome::Draw) => 0,
                None => 0,
            })),
            true => None,
        }
    }

    fn outcome(&mut self, outcome: Option<pgn_reader::Outcome>) {
        if let Some(outcome) = outcome {
            self.outcome = Some(outcome);
        }
    }
}

fn role_to_piece(role: Role) -> Piece {
    match role {
        Role::Pawn =>   Piece::Pawn,
        Role::Knight => Piece::Knight,
        Role::Bishop => Piece::Bishop,
        Role::Rook =>   Piece::Rook,
        Role::Queen =>  Piece::Queen,
        Role::King =>   Piece::King,
    }
}

fn parse_san(board: &Board, san_plus: pgn_reader::SanPlus) -> Option<Move> {
    let is_white_actv = board.colour == 1;
    match san_plus.san {
        pgn_reader::San::Normal { role, file, rank, capture, to, promotion } => {
            let mut tab = infra::board::mov::MoveSetTable::new();
            board.get_role_move_tab_actv(role_to_piece(role), &mut tab);

            for_mov!(mov in tab => {
                if board.is_move_legal(mov) {
                    let to_sq = if is_white_actv { mov.to_sq } else { flip_sq(mov.to_sq) };
                    if to_sq % 8 == to.file().char() as u8 - b'a' {
                        if to_sq / 8 == to.rank().char() as u8 - b'1' {
                            if let Some(prom) = promotion {
                                if mov.piece != role_to_piece(prom) { continue; }
                            }
                            if capture {
                                if board.get_piece_at(1 << mov.to_sq).is_none() 
                                && board.en_passant & (1 << mov.to_sq) == 0 { continue; }
                            }
                            let from_sq = if is_white_actv { mov.from_sq } else { flip_sq(mov.from_sq) };
                            if let Some(from_file) = file {
                                if from_sq % 8 != from_file.char() as u8 - b'a' { continue; }
                            }
                            if let Some(from_rank) = rank {
                                if from_sq / 8 != from_rank.char() as u8 - b'1' { continue; }
                            }

                            return Some(mov);
                        }
                    }
                }
            });
        },
        pgn_reader::San::Put { role, to } => {
            let mut tab = infra::board::mov::MoveSetTable::new();
            board.get_role_move_tab_actv(role_to_piece(role), &mut tab);

            for_mov!(mov in tab => {
                if board.is_move_legal(mov) {
                    let to_sq = if is_white_actv { mov.to_sq } else { flip_sq(mov.to_sq) };
                    if to_sq % 8 == to.file().char() as u8 - b'a' {
                        if to_sq / 8 == to.rank().char() as u8 - b'1' {
                            return Some(mov);
                        }
                    }
                }
            });
        },
        pgn_reader::San::Castle(side) => {
            let mov;
            match side {
                pgn_reader::CastlingSide::KingSide => {
                    mov = Move::new(4, 6, Piece::King);
                },
                pgn_reader::CastlingSide::QueenSide => {
                    mov = Move::new(4, 2, Piece::King);
                },
            }

            
            let mut tab = infra::board::mov::MoveSetTable::new();
            board.get_role_move_tab_actv(Piece::King, &mut tab);

            for_mov!(king_move in tab => {
                if board.is_move_legal(king_move) {
                    if mov == king_move {
                        return Some(king_move);
                    }
                }
            });
        },
        pgn_reader::San::Null => eprintln!("Null move?"),
    }

    None
}

