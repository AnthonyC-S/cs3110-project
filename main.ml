open Tile
open Player
open State
open Command
open Textgui
open Board

(** Need Main Module Description *)

(* Following messages are for caught exceptions. *)
let have_not_played_meld_msg =
  "  You cannot move board tiles until you have played a 30 point meld.\n"

let invalid_board_sets_msg =
  "  You cannot end turn due to invalid sets on the board.\n\
  \  Try to fix the invaid sets or go back with \"undo\" / \"reset\".\n"

let invalid_index_msg i =
  "  Could not find the tile on "
  ^ (if fst i = "" then "rack" else "row " ^ fst i)
  ^ ", index "
  ^ string_of_int (snd i)
  ^ ". Check if the tile has the correct\n\
    \  index and/or row. Type \"help\" to see commands.\n"

let invalid_tile_msg =
  "  Could not find the tile you entered. Check if the tile has the \
   correct\n\
  \  index and/or row. Type \"help\" to see commands.\n"

let invalid_board_row_msg s =
  "  Could not find the board row " ^ s
  ^ ". Check the capitalization of the row name. Type \"help\" to see \
     commands.\n"

let invalid_meld_msg =
  "  Can not end turn since since you do not have a valid meld, i.e. \
   30 points or higher.\n\n\
  \  You can either play more tiles to make a meld or undo the tiles \
   and draw to end your turn.\n"

let not_enough_tiles_msg =
  "  The pile is empty and there are no tiles left to draw.\n\
  \  There should be enough tiles to finish the game!\n"

let row_already_full_msg s =
  "  Board row " ^ s
  ^ " can not have more than 13 tiles. Try adding fewer tiles or \
     adding to a new row.\n"

let game_commands =
  g "  Game Commands:\n\n"
  ^ ip ^ "move t to r       Moves the tile(s) [t] to board row [r].\n"
  ^ g "  For Example:\n" ^ ip
  ^ "move 1 4 A3 to B  Moves rack tiles at index 1 and 4, and board \
     row A tile at index 3, to row B.\n" ^ ip
  ^ "undo              Undo most recent move.\n" ^ ip
  ^ "reset             Resets board and rack to start of turn.\n" ^ ip
  ^ "color sort        Sorts rack by tile color.\n" ^ ip
  ^ "number sort       Sorts rack by tile number.\n" ^ ip
  ^ "draw              Draws a new tile and ends turn.\n" ^ ip
  ^ "end turn          Checks for a valid board and ends turn.\n" ^ ip
  ^ "help              Display game play commands.\n\n"

let malformed_msg =
  "  Did you enter the command correctly? Type \"help\" for commands.\n"

let empty_move_msg =
  "  The move command is empty. Type \"help\" to see the move command \
   structure.\n"

let empty_move_from_msg =
  "  The move command does not have tiles to move. Type \"help\" to \
   see the move command structure.\n"

let empty_move_to_msg =
  "  The move command is missing a row to move the tiles to. Type \
   \"help\" to see the move command structure.\n"

let invalid_move_missing_to_msg =
  "  The move command is missing \"to\". Type \"help\" to see the move \
   command structure.\n"

let rec str_lst_syntax (str_lst : string list) : string =
  match str_lst with
  | [] -> ""
  | [ h ] -> "\"" ^ g h ^ "\""
  | [ h; t ] -> "\"" ^ g h ^ "\"" ^ " and " ^ "\"" ^ g t ^ "\""
  | h :: t -> "\"" ^ g h ^ "\"" ^ ", " ^ str_lst_syntax t

let invalid_move_from_msg str_lst =
  "  Could not find " ^ str_lst_syntax str_lst
  ^ ".\n  Double check the tile"
  ^ (if List.length str_lst = 1 then "" else "s")
  ^ " you are trying to move. Type \"help\" to see commands.\n"

let invalid_move_to_msg str_lst =
  "  Could not move to " ^ str_lst_syntax str_lst
  ^ ".\n  Double check the row name you are trying to move to. "
  ^
  if List.length str_lst = 1 then "Type \"help\" to see commands.\n"
  else
    "You can only move to a single row.\n\
    \  Type \"help\" to see commands.\n"

let duplicate_move_from_msg str_lst =
  "  You are trying to move the tile"
  ^ (if List.length str_lst = 1 then " at position "
    else "s at positions ")
  ^ str_lst_syntax str_lst
  ^ " twice.\n\
    \  Each tile you move must have a unique location. Type \"help\" \
     to see commands.\n"

let multiple_move_to_msg str_lst =
  "  You are trying to move to these locations: "
  ^ str_lst_syntax str_lst
  ^ ".\n\
    \  But you can only move to a single row. Type \"help\" to see \
     commands.\n"

let draw_msg = "  Drawed tile from pile. Type \"end turn\".\n"

let end_turn_msg = "  Starting next players turn.\n"

let sort_num_msg = "  Sorted by number.\n"

let sort_col_msg = "  Sorted by color.\n"

let quit_game () =
  print_string ("\n" ^ g "  Thank you for playing, goodbye!\n\n");
  Stdlib.exit 0

let reset_msg st =
  if st.past_state = [] then "  No moves to go back to.\n"
  else "  Board and rack have been reset.\n"

let undo_msg st =
  if st.past_state = [] then "  No moves to go back to.\n"
  else "  Went back one move.\n"

let already_drawn_msg = function
  | "undo" -> "  Tile already drawn. Cannot undo. Type \"end turn\".\n"
  | "reset" ->
      "  Tile already drawn. Cannot reset. Type \"end turn\".\n"
  | "move after drawn" ->
      "  Tile already drawn. Cannot move any tiles. Type \"end turn\".\n"
  | "draw again" ->
      "  Tile already drawn. Cannot draw again. Type \"end turn\".\n"
  | _ -> failwith "Invalid AlreadyDrawn exception"

let already_moved_msg =
  "  You have already made a move. Reset all moves to draw.\n"

let get_exception_msg = function
  | EmptyMove -> empty_move_msg
  | EmptyMoveFrom -> empty_move_from_msg
  | EmptyMoveTo -> empty_move_to_msg
  | InvalidMoveMissingTo -> invalid_move_missing_to_msg
  | InvalidMoveFrom s -> invalid_move_from_msg s
  | InvalidMoveTo s -> invalid_move_to_msg s
  | DuplicateMoveFrom s -> duplicate_move_from_msg s
  | MultipleMoveTo s -> multiple_move_to_msg s
  | HaveNotPlayedMeld -> have_not_played_meld_msg
  | InvalidBoardSets -> invalid_board_sets_msg
  | InvalidIndex i -> invalid_index_msg i
  | InvalidTile -> invalid_tile_msg
  | InvalidBoardRow s -> invalid_board_row_msg s
  | InvalidMeld -> invalid_meld_msg
  | NotEnoughTiles -> not_enough_tiles_msg
  | RowAlreadyFull s -> row_already_full_msg s
  | AlreadyDrawn s -> already_drawn_msg s
  | AlreadyMoved -> already_moved_msg
  | Malformed | BlankInput | _ -> malformed_msg

let rec play_turn st msg =
  clear_board ();
  print_string (build_board st msg);
  match read_line () with
  | exception End_of_file -> ()
  | str -> (
      try
        let command = parse str in
        commands command st
      with e -> play_turn st (get_exception_msg e))

and commands command st =
  try
    match command with
    | Quit -> quit_game ()
    | Undo -> play_turn (undo_move st) (undo_msg st)
    | Move m -> play_turn (move m st) "  Completed move, what next?\n"
    | Reset -> play_turn (reset_turn st) (reset_msg st)
    | SortByColor -> play_turn (sort_rack_by_color st) sort_col_msg
    | SortByNumber -> play_turn (sort_rack_by_num st) sort_num_msg
    | Draw -> play_turn (draw st) draw_msg
    | EndTurn -> play_turn (end_turn st) end_turn_msg
    | Help -> play_turn st game_commands
  with e -> play_turn st (get_exception_msg e)

let rec welcome st msg =
  clear_board ();
  print_string welcome_board;
  print_string msg;
  print_string
    (g
       "  Let's start! Type \"play\" to start game or \"quit\" to exit.\n"
    ^ ip);
  match read_line () with
  | input ->
      if input = "play" || input = "p" then
        play_turn st "  Enter your command to play.\n"
      else if input = "quit" then quit_game ()
      else welcome st game_commands

let repeat_init_game_aux () =
  print_string ("  Try again or type \"quit\" to exit.\n" ^ ip);
  match read_line () with
  | "quit" -> quit_game ()
  | init_game -> init_game

(* [game_start] attempts to initilize state using entered number of
   players and player names. *)
let rec game_start str =
  try welcome (init_state (parse_start str)) game_commands with
  | NameTooLong ->
      print_string
        (g "\n  Player names must be shorter than 20 characters.\n");
      game_start (repeat_init_game_aux ())
  | NotUniqueNames ->
      print_string (g "\n  Each player's name must be unique.\n");
      game_start (repeat_init_game_aux ())
  | Malformed ->
      print_string
        (g "\n  The start game command was not entered correctly.\n");
      game_start (repeat_init_game_aux ())

(* [main] is the initial welcome screen and has player input in number
   of players and player names to start a new game. *)
let main () =
  ANSITerminal.resize 107 45;
  ANSITerminal.set_cursor 1 1;
  ANSITerminal.erase Screen;
  print_string
    (welcome_board
    ^ g
        "  Enter the number of players (2 or 4) and, optionally, the \
         player names. For example:\n"
    ^ ip ^ "4 Clarkson Gries Dijkstra Turing\n" ^ ip);
  match read_line () with init_game -> game_start init_game

(* Execute the game engine. *)
let () = main ()
