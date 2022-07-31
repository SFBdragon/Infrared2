use std::{sync::{Arc, atomic::{AtomicBool, Ordering}}, thread, io::Write};

use infra::{Move, Board, Piece, SearchInfo, SearchEval, TransTable, Game, SearchHandle, Side};
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
    // Initialize opening book
    let _ = infra::opening::query_book(0);

    // Transposition table is only reset on newgames
    let mut trans_table = Arc::new(TransTable::default());

    let mut search_handle: Option<SearchHandle> = None;

    // UCI requires that the position be recalled across messages
    let mut position: Option<Game> = None;
    // UCI hence requires that a bestmove be declarable on-demand
    let mut best_move = Option::<UciMove>::None;


    // Stdin thread should block upon receiving lots of input
    let (stdin_sndr, stdin_rcvr) = crossbeam_channel::bounded::<String>(0);
    let _stdin_handle = thread::spawn(move || read_stdin(stdin_sndr));

    
    'event: loop {
        let selector = crossbeam_channel::Select::new();
        let stdin_index = selector.recv(&stdin_rcvr);
        let info_index = search_handle.map(|sh| selector.recv(&sh.info_receiver));
        let operation_index = selector.ready();

        if info_index.is_some() && operation_index == info_index.unwrap() {
            if let Ok((info, is_final)) = search_handle.unwrap().info_receiver.try_recv() {
                best_move = uci_search_info(&position.as_ref().unwrap().position, info, is_final); // todo: fix?
            }
        } else if operation_index == stdin_index {
            if let Ok(input) = stdin_rcvr.try_recv() {

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
                            if let Some(sh) = search_handle {
                                sh.kill_switch.store(true, Ordering::SeqCst);
                            }
                            search_handle = None;
                            position = None;
                            best_move = None;
                            trans_table = Arc::new(TransTable::default());
                        }
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
            }
        } else {
            panic!()
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

fn uci_position(startpos: bool, fen: Option<UciFen>, moves: Vec<UciMove>) -> Option<Game> {
    let mut board = if startpos {
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
    
    if let Some(begin_pos) = board {
        Some(Game::with_coded(begin_pos, moves, |m, b| from_uci_move(b, *m).unwrap()).unwrap())
    } else {
        eprintln!("Invalid position message!");
        None
    }
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

fn uci_search_info(position: &Board, info: SearchInfo, is_final: bool) -> Option<UciMove> {
    let (pv_move, pv_eval) = info.evals[0];

    // send 'info' message
    print!("{}\n", UciMessage::Info(vec![
        // send principal variation
        UciInfoAttribute::Pv(vec![to_uci_move(&position, pv_move)]),
        // send score
        UciInfoAttribute::Score { 
            cp: if let SearchEval::Normal(score) = pv_eval { Some(score as i32) } else { None },
            mate: if let SearchEval::Mate(depth) = pv_eval { Some(depth) } else { None }, 
            lower_bound: None, 
            upper_bound: None 
        },
    ]));

    if is_final { // send 'bestmove'
        print!("{}\n", UciMessage::BestMove {
            best_move: to_uci_move(position, pv_move),
            ponder: None
        });
        std::io::stdout().flush().expect("stdout flush error");

        // reset provisional best_move
        None 
    } else { // else only update provisional best_move
        Some(to_uci_move(&position, pv_move)) 
    }
}


// Helpers

fn to_uci_move(board: &Board, mov: Move) -> UciMove {
    let is_white_to_play = board.colour == Side::White;
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
    let is_white_to_play = board.colour == Side::White;
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


