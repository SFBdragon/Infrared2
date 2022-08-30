use std::{sync::{Arc, atomic::Ordering}, thread, io::Write};
use crossbeam_channel::{Sender, Receiver};
use vampirc_uci::{
    UciMessage, 
    CommunicationDirection,
    UciMove, 
    UciPiece, 
    UciSquare, 
    UciInfoAttribute, UciFen, UciTimeControl,
};

use infra::{Move, Board, Piece, SearchInfo, SearchEval, Game, SearchHandle, Side, opening, TimeControl, Sq, search::htab::{TransTable, self, PkEvalTable}};


struct SearchControl {
    search_handle: SearchHandle,
    syzygy_rcvr: Receiver<SearchInfo>,
}


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
    let mut trans_table = Arc::new(TransTable::with_memory(htab::TRANS_MEM_DEFAULT));
    let mut pk_eval_table = Arc::new(PkEvalTable::with_memory(htab::PK_EVAL_MEM_DEFAULT));

    let mut search_control: Option<SearchControl> = None;

    // UCI requires that the position be recalled across messages
    let mut position: Option<Game> = None;


    // Stdin thread should block upon receiving lots of input
    let (stdin_sndr, stdin_rcvr) = crossbeam_channel::bounded::<String>(0);
    let _stdin_handle = thread::spawn(move || read_stdin(stdin_sndr));

    
    'event: loop {
        let mut selector = crossbeam_channel::Select::new();
        let stdin_index = selector.recv(&stdin_rcvr);
        let mut search_info_index = None;
        let mut syzygy_info_index = None;
        if let Some(sc) = search_control.as_ref() {
            search_info_index = Some(selector.recv(&sc.search_handle.info_channel.1));
            syzygy_info_index = Some(selector.recv(&sc.syzygy_rcvr));
        }
        let operation_index = selector.ready();

        if search_info_index.is_some() && operation_index == search_info_index.unwrap() {
            let search_control_ref = search_control.as_ref().unwrap();
            if let Ok((info, is_final)) = search_control_ref.search_handle.info_channel.1.try_recv() {
                uci_search_info(&position.as_ref().unwrap().position, info, is_final);
                if is_final {
                    search_control_ref.search_handle.kill_switch.store(true, Ordering::SeqCst);                
                    search_control = None;
                }
            }
        } else if syzygy_info_index.is_some() && operation_index == syzygy_info_index.unwrap() {
            let search_control_ref = search_control.as_ref().unwrap();
            if let Ok(info) = search_control_ref.syzygy_rcvr.try_recv() {
                uci_search_info(&position.as_ref().unwrap().position, info, true);
                search_control_ref.search_handle.kill_switch.store(true, Ordering::SeqCst);
                search_control = None;
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
                            if let Some(sc) = std::mem::replace(&mut search_control, None) {
                                sc.search_handle.kill_switch.store(true, Ordering::SeqCst);
                            }
                            position = None;
                            trans_table = Arc::new(TransTable::with_memory(htab::TRANS_MEM_DEFAULT));
                            pk_eval_table = Arc::new(PkEvalTable::with_memory(htab::PK_EVAL_MEM_DEFAULT));
                        }
                        UciMessage::Position { startpos, fen, moves } => {
                            search_control = None;
                            position = uci_position(startpos, fen, moves)
                        }
                        UciMessage::Go { time_control, search_control: _ } => {
                            search_control = None;

                            if let Some(game) = &position {
                                // syzygy query thread (auto checks piece count)
                                let syzygy_board = game.position.clone();
                                let (syzygy_sndr, syzygy_rcvr) = crossbeam_channel::bounded(1);
                                thread::spawn(move || {
                                    let syzygy_board = syzygy_board;
                                    infra::syzygy::uci_syzygy_query(
                                        &syzygy_board,
                                        syzygy_sndr,
                                    );
                                });
    
                                if let Some(mov) = opening::query_book_best(game.position.hash) {
                                    // opening book!
                                    print!("{}\n", UciMessage::BestMove {
                                        best_move: to_uci_move(&game.position, mov),
                                        ponder: None,
                                    }.to_string());
                                    std::io::stdout().flush().expect("stdout flush error");
                                } else {
                                    let time_control = if let Some(tc) = time_control {
                                        to_uci_time_control(tc, game.position.side)
                                    } else {
                                        TimeControl::Infinite
                                    };

                                    // search!
                                    let search_handle = game.search(
                                        time_control, 
                                        Some(trans_table.clone()),
                                        Some(pk_eval_table.clone()),
                                    );

                                    search_control = Some(SearchControl {
                                        search_handle,
                                        syzygy_rcvr,
                                    });
                                }
                            }
                        }
                        UciMessage::Stop => {
                            if let Some(sc) = search_control.as_ref() {
                                sc.search_handle.kill_switch.store(true, Ordering::SeqCst);
                            }
                        }
                        UciMessage::Quit => {
                            if let Some(sc) = std::mem::replace(&mut search_control, None) {
                                sc.search_handle.kill_switch.store(true, Ordering::SeqCst);
                            }
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
    let board = if startpos {
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


fn uci_search_info(position: &Board, info: SearchInfo, is_final: bool) {
    let mov = info.pv[0];

    if is_final { // send 'bestmove'
        print!("{}\n", UciMessage::BestMove {
            best_move: to_uci_move(position, mov),
            ponder: None
        });
        std::io::stdout().flush().expect("stdout flush error");
    }

    // send info message
    let mut uci_info = vec![UciInfoAttribute::Pv(vec![to_uci_move(&position, mov)])];
    if let Some(eval) = info.eval {
        uci_info.push(UciInfoAttribute::Score { 
            cp: if let SearchEval::Normal(score) = eval { Some(score as i32) } else { None },
            mate: if let SearchEval::Mate(depth) = eval { Some(depth) } else { None }, 
            lower_bound: None, 
            upper_bound: None 
        });
    }
    if let Some((depth, sel_depth)) = info.depth {
        uci_info.push(UciInfoAttribute::Depth(depth as u8));
        uci_info.push(UciInfoAttribute::SelDepth(sel_depth as u8));
    }
    print!("{}\n", UciMessage::Info(uci_info).to_string());
}


// Helpers

fn to_uci_move(board: &Board, mv: Move) -> UciMove {
    UciMove {
        from: UciSquare {
            file: (mv.from.file() + b'a') as char,
            rank: mv.from.cflip(board.side).rank() + 1,
        },
        to: UciSquare {
            file: (mv.to.file() + b'a') as char,
            rank: mv.to.cflip(board.side).rank() + 1,
        },
        promotion: if mv.piece != Piece::Pawn && mv.from.bm() & board.pawns != 0 {
            Some(to_uci_piece(mv.piece))
        } else {
            None
        },
    }
}

fn from_uci_move(board: &Board, mov: UciMove) -> Option<Move> {
    let from = Sq::file_rank(mov.from.file as u8 - b'a', mov.from.rank - 1).cflip(board.side);
    let to   = Sq::file_rank(mov.to.file as u8 - b'a',   mov.to.rank - 1  ).cflip(board.side);

    let piece = match mov.promotion {
        Some(piece) => from_uci_piece(piece),
        None => board.get_piece_at(from)?,
    };
    
    let mov = Move::new(from, to, piece);

    match board.is_valid(mov) {
        true => Some(mov),
        false => None,
    }
}

fn to_uci_time_control(time_control: UciTimeControl, side: Side) -> TimeControl {
    match time_control {
        UciTimeControl::Ponder => panic!("ponder request?"),
        UciTimeControl::Infinite => TimeControl::Infinite,
        UciTimeControl::MoveTime(time) => TimeControl::MoveTime(time.to_std().unwrap()),
        UciTimeControl::TimeLeft { white_time, black_time, white_increment, black_increment, moves_to_go } => {
            match side {
                Side::White => TimeControl::TimeLeft {
                    time_left: white_time.unwrap().to_std().unwrap(),
                    increment: white_increment.map(|d| d.to_std().unwrap()),
                    moves_left: moves_to_go.map(|c| c as usize),
                },
                Side::Black => TimeControl::TimeLeft {
                    time_left: black_time.unwrap().to_std().unwrap(),
                    increment: black_increment.map(|d| d.to_std().unwrap()),
                    moves_left: moves_to_go.map(|c| c as usize),
                },
            }
        },
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


