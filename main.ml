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

let invalid_board_row_msg =
  "  Could not find the row you entered. Check the capitalization of \
   the row name. Type \"help\" to see commands.\n"

let invalid_meld_msg =
  "  Can not end turn since since you do not have a valid meld, i.e. \
   30 points or higher.\n\n\
  \  You can either play more tiles to make a meld or undo the tiles \
   and draw to end your turn.\n"

let not_enough_tiles_msg =
  "  The pile is empty and there are no tiles left to draw.\n\
  \  There should be enough tiles to finish the game!\n"

let row_already_full_msg =
  "  Board rows can not have more than 13 tiles. Try adding fewer \
   tiles or adding to a new row.\n"

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

let quit_game () =
  print_string ("\n" ^ g "  Thank you for playing, goodbye!\n\n");
  Stdlib.exit 0

let rec play_turn st msg =
  clear_board ();
  print_string (build_board st msg);
  match read_line () with
  | exception End_of_file -> ()
  | str -> (
      try
        let command = parse str in
        commands command st
      with Malformed | BlankInput ->
        play_turn st
          "  Did you enter the command correctly? Type \"help\" for \
           commands.\n")

and commands command st =
  try
    match command with
    | Quit -> quit_game ()
    | Undo ->
        if get_past_racks st.current_turn st.players = [] then
          play_turn st "  No moves to go back to.\n"
        else play_turn (undo_move st) "  Went back one move.\n"
    | Move m ->
        play_turn
          (multiple_moves_from_board m.from_board m.to_row st
          |> multiple_moves_from_rack m.from_rack m.to_row)
          "  Completed move, what next?\n"
    | Reset ->
        if st.past_boards = [] then
          play_turn st "  No moves to go back to.\n"
        else
          play_turn (reset_turn st)
            "  Board and rack have been reset.\n"
    | SortByColor ->
        play_turn (sort_rack_by_color st) "  Sorted by color.\n"
    | SortByNumber ->
        play_turn (sort_rack_by_number st) "  Sorted by number.\n"
    | Draw ->
        play_turn (draw st)
          "  Drawed tile from pile. Type \"end turn\".\n"
    | EndTurn ->
        play_turn (end_turn st) "  Starting next players turn.\n"
    | Help -> play_turn st game_commands
  with
  | HaveNotPlayedMeld -> play_turn st have_not_played_meld_msg
  | InvalidBoardSets -> play_turn st invalid_board_sets_msg
  | InvalidIndex i -> play_turn st (invalid_index_msg i)
  | InvalidTile -> play_turn st invalid_tile_msg
  | InvalidBoardRow -> play_turn st invalid_board_row_msg
  | InvalidMeld -> play_turn st invalid_meld_msg
  | NotEnoughTiles -> play_turn st not_enough_tiles_msg
  | RowAlreadyFull -> play_turn st row_already_full_msg

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
