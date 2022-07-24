use std::{sync::{Arc, atomic::{AtomicBool, Ordering}}, thread, io::Write};

use infra::{Move, Board, Piece, search::{SearchInfo, ttab, SearchEval}};
use crossbeam_channel::Sender;
use vampirc_uci::{
    UciMessage, 
    CommunicationDirection,
    UciMove, 
    UciPiece, 
    UciSquare, 
    UciInfoAttribute, UciFen,
};

fn read_stdin(sender: Sender<String>) {
    loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).expect("Stdin Error!");
        if let Err(_) = sender.send(buffer) { return; }
    }
}

pub fn uci() {
    // init opening book
    let _ = infra::opening::query_book(0);

    // UCI requires that a position be recalled across messages
    let mut position = Option::<Board>::None;
    // UCI requires that the engine is stop-able at any time
    let kill_switch = Arc::new(AtomicBool::new(false));
    // UCI hence requires that a bestmove be declarable on-demand
    let mut best_move = Option::<UciMove>::None;

    let /* mut */ trans_table = Option::<Arc<ttab::TransTable>>::Some(Arc::new(ttab::TransTable::default()));
    let /* mut */ pos_hash_map = Arc::new(infra::PosHashMap::with_hasher(infra::U64IdentHashBuilder));

    // Stdin thread should block upon receiving lots of input
    let (stdin_sndr, stdin_rcvr) = crossbeam_channel::bounded::<String>(0);
    let _stdin_handle = thread::spawn(move || read_stdin(stdin_sndr));

    // Engine should never be blocked when sending info
    let (search_sndr, search_rcvr) = crossbeam_channel::unbounded::<(SearchInfo, bool)>();
    
    'event: loop {
        crossbeam_channel::select! {
            recv(search_rcvr) -> search_info => {
                let search_info = search_info.expect("lost info channel!");
                best_move = uci_search_info(position.as_ref().unwrap(), search_info.0, search_info.1); // todo: fixme
            }
            recv(stdin_rcvr) -> input => {
                let input = input.expect("lost stdin channel!");
                
                // http://wbec-ridderkerk.nl/html/UCIProtocol.html
                for message in vampirc_uci::parse_with_unknown(input.as_str()) {
                    match message {
                        UciMessage::Uci => {
                            uci_init()
                        }
                        UciMessage::IsReady => {
                            print!("{}\n", UciMessage::ReadyOk.to_string())
                        }
                        UciMessage::Register { later: _, name: _, code: _ } => {
                            uci_register()
                        }
                        UciMessage::UciNewGame => {
                            kill_switch.store(true, Ordering::SeqCst);
                            position = None;
                            //persistent = None;
                            best_move = None;
                        },
                        UciMessage::Position { startpos, fen, moves } => {
                            position = uci_position(startpos, fen, moves)
                        }
                        UciMessage::Go { time_control: _, search_control: _ } => {
                            position.as_ref().unwrap().validate().unwrap();

                            

                            // syzygy query thread (auto checks piece count)
                            let pos = position.clone().unwrap();
                            let sis = search_sndr.clone();
                            thread::spawn(move ||
                                infra::syzygy::query_table_best_uci(
                                    pos,
                                    sis,
                                )
                            );

                            if let Some(mov) = infra::opening::query_book_best(position.as_ref().unwrap().hash) {
                                // opening book!
                                print!("{}\n", UciMessage::BestMove {
                                    best_move: to_uci_move(position.as_ref().unwrap(), mov),
                                    ponder: None,
                                }.to_string());
                                std::io::stdout().flush().expect("stdout flush error");
                            } else {
                                // search!
                                kill_switch.store(false, Ordering::SeqCst);
                                let pos = position.clone().unwrap();
                                let ks = kill_switch.clone();
                                let sis = search_sndr.clone();
                                let ttab = trans_table.as_ref().unwrap().clone();
                                let phn = pos_hash_map.clone();
    
                                thread::spawn(move ||
                                    infra::search::search(
                                        pos, 
                                        sis, 
                                        ttab,
                                        phn,
                                        ks
                                    )
                                );
                            }
                        }
                        UciMessage::Stop => {
                            uci_stop(best_move, None, kill_switch.as_ref());
                        }
                        UciMessage::Quit => {
                            kill_switch.store(true, Ordering::SeqCst);
                            break 'event;
                        }
                        UciMessage::SetOption { name: _, value: _ } => (),
                        UciMessage::PonderHit => (),
                        UciMessage::Debug(_) => (),
                        UciMessage::Unknown(_, _) => {
                            eprintln!("Unknown message! \'{input}\'");
                        }
                        message => if message.direction() != CommunicationDirection::EngineToGui {
                            eprintln!("Unhandled Engine-bound UCI message!")
                        },
                    }
                }
            },
        }
    }
}


// Handlers

fn uci_init() {
    // identify self
    print!("{}\n", UciMessage::Id {
        name: Some("Infrared 2".to_owned()),
        author: Some("Shaun Beautement".to_owned()),
    });

    // define options
    /* print!("{}", UciMessage::Option(UciOptionConfig::Spin {
        name: "Hash".to_owned(),
        default: Some(1024),
        min: Some(0),
        max: None,
    })); */

    // uciok
    print!("{}\n", UciMessage::UciOk.to_string());
}

fn uci_register() {
    use vampirc_uci::ProtectionState;

    // no registration mechanism; always ok
    print!("{}\n", UciMessage::Registration(ProtectionState::Checking).to_string());
    print!("{}\n", UciMessage::Registration(ProtectionState::Ok).to_string());
}

fn uci_position(startpos: bool, fen: Option<UciFen>, moves: Vec<UciMove>) -> Option<Board> {
    let mut position = if startpos {
        Some(Board::default())
    } else if let Some(fen) = fen {
        match Board::from_fen(fen.as_str()) {
            Ok(board) => Some(board),
            Err(err) => {
                eprintln!("FEN Parse Error: \'{}\'", err);
                None
            },
        }
    } else {
        None
    };
    
    if let Some(pos) = (&mut position).as_mut() {
        for uci_mov in moves {
            if let Some(mov) = from_uci_move(pos, uci_mov) {
                pos.make(mov);
            } else {
                eprintln!("Invalid position move!");
            }
        }
    } else {
        eprintln!("Invalid position message!")
    }

    position
}


fn uci_stop(best: Option<UciMove>, ponder: Option<UciMove>, kill_switch: &AtomicBool) {
    // kill the engine
    kill_switch.store(true, Ordering::SeqCst);

    // bestmove
    if let Some(best_move) = best {
        print!("{}\n", UciMessage::BestMove {
            best_move: best_move,
            ponder: ponder, 
        });
    } else {
        eprintln!("\'stop\' issued without a best move!");
    }
}

fn uci_search_info(position: &Board, info: SearchInfo, done: bool) -> Option<UciMove> {
    // send 'info' message
    print!("{}\n", UciMessage::Info(vec![
        // send principal variation
        UciInfoAttribute::Pv(vec![to_uci_move(&position, info.best)]),
        // send score
        UciInfoAttribute::Score { 
            cp: if let SearchEval::Normal(score) = info.eval { Some(score as i32) } else { None },
            mate: if let SearchEval::Mate(depth) = info.eval { Some(depth) } else { None }, 
            lower_bound: None, 
            upper_bound: None 
        },
    ]));

    if done { // send 'bestmove'
        print!("{}\n", UciMessage::BestMove {
            best_move: to_uci_move(position, info.best),
            ponder: None
        });
        std::io::stdout().flush().expect("stdout flush error");

        // reset provisional best_move
        None 
    } else { // else only update provisional best_move
        Some(to_uci_move(&position, info.best)) 
    }
}


// Helpers

fn to_uci_move(board: &Board, mov: Move) -> UciMove {
    let is_white_to_play = board.colour == 1;
    UciMove {
        from: UciSquare {
            file: char::from_u32((b'a' + mov.from_sq % 8) as u32).unwrap(),
            rank: if is_white_to_play { (mov.from_sq / 8) + 1 } else { 8 - (mov.from_sq / 8) },
        },
        to: UciSquare {
            file: char::from_u32((b'a' + mov.to_sq % 8) as u32).unwrap(),
            rank: if is_white_to_play { (mov.to_sq / 8) + 1 } else { 8 - (mov.to_sq / 8) },
        },
        promotion: if mov.piece != Piece::Pawn && (1 << mov.from_sq) & board.pawns != 0 {
            Some(to_uci_piece(mov.piece))
        } else {
            None
        },
    }
}

fn from_uci_move(board: &Board, mov: UciMove) -> Option<Move> {
    let is_white_to_play = board.colour == 1;
    let mut from_sq = (mov.from.file as u8 - b'a') + (mov.from.rank - 1) * 8;
    let mut to_sq   = (mov.to.file as u8 - b'a')   + (mov.to.rank   - 1) * 8;
    if !is_white_to_play {
        from_sq = infra::board::flip_sq(from_sq);
        to_sq = infra::board::flip_sq(to_sq);
    }

    let piece = match mov.promotion {
        Some(piece) => from_uci_piece(piece),
        None => board.get_piece_at(1 << from_sq)?,
    };
    
    let mov = Move::new(from_sq, to_sq, piece);

    match board.is_valid(mov) {
        true => Some(mov),
        false => None,
    }
}

fn to_uci_piece(piece: Piece) -> UciPiece {
    match piece {
        Piece::Pawn => UciPiece::Pawn,
        Piece::Knight => UciPiece::Knight,
        Piece::Bishop => UciPiece::Bishop,
        Piece::Rook => UciPiece::Rook,
        Piece::Queen => UciPiece::Queen,
        Piece::King => UciPiece::King,
    }
}
fn from_uci_piece(piece: UciPiece) -> Piece {
    match piece {
        UciPiece::Pawn =>   Piece::Pawn,
        UciPiece::Knight => Piece::Knight,
        UciPiece::Bishop => Piece::Bishop,
        UciPiece::Rook =>   Piece::Rook,
        UciPiece::Queen =>  Piece::Queen,
        UciPiece::King =>   Piece::King,
    }
}


