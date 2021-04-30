open Tile
open Player
open State
open Command
open Textgui
open Board

(** Need Main Module Description *)

(***********************************************************************)
(* Following functions provide a message are for caught exceptions.    *)
(***********************************************************************)

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
  "  Cannot end turn since you do not have a valid meld of 30 tile \
   points or higher.\n\
  \  You can either play more tiles to make a meld or undo the tiles \
   and draw to end your turn.\n"

let not_enough_tiles_msg =
  "  The pile is empty and there are no tiles left to draw.\n\
  \  There should be enough tiles to finish the game!\n"

let row_already_full_msg s =
  "  Board row " ^ s
  ^ " cannot have more than 13 tiles. Try adding fewer tiles or adding \
     to a new row.\n"

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
  ^ " multiple times.\n\
    \  Each tile you move must have a unique location. Type \"help\" \
     to see commands.\n"

let multiple_move_to_msg str_lst =
  "  You are trying to move to these locations: "
  ^ str_lst_syntax str_lst
  ^ ".\n\
    \  But you can only move to a single row. Type \"help\" to see \
     commands.\n"

(***********************************************************************)
(* Following functions provide a message for executed commands.        *)
(***********************************************************************)
let move_msg = "  Completed move, what next?\n"

let undo_msg st =
  if st.past_state = [] then "  No moves to go back to.\n"
  else "  Went back one move.\n"

let reset_msg st =
  if st.past_state = [] then "  No moves to go back to.\n"
  else "  Board and rack have been reset.\n"

let sort_col_msg = "  Sorted by color.\n"

let sort_num_msg = "  Sorted by number.\n"

let draw_msg = "  Drawed tile from pile. Type \"end turn\".\n"

let end_turn_msg st =
  if
    (not (get_current_player st).drawn_current_turn)
    && st.past_state = []
  then
    "  You cannot end turn without making any move. Type \"help\" to \
     see commands.\n"
  else "  Starting next players turn.\n"

let spaces_scores n = String.make (10 - n) ' '

let spaces_name player_lst n =
  let max_name_len =
    List.map (fun x -> String.length x.name) player_lst
    |> List.fold_left max 0
  in
  String.make (6 + max_name_len - n) ' '

let string_of_scores player_lst =
  let rec aux acc i player_num =
    if i = player_num + 1 then acc
    else
      let cur_player = player_to_update i player_lst in
      aux
        (acc ^ "  " ^ cur_player.name
        ^ spaces_name player_lst (String.length cur_player.name)
        ^ (List.map
             (fun x ->
               let s = string_of_int x in
               s ^ spaces_scores (String.length s))
             (List.rev cur_player.score)
          |> String.concat "")
        ^ string_of_int (List.fold_left ( + ) 0 cur_player.score)
        ^ "\n")
        (i + 1) player_num
  in
  aux "" 1 (List.length player_lst)

let score_msg player_lst =
  let num_games = List.length (List.hd player_lst).score in
  let num_games_row =
    g "  Name"
    ^ spaces_name player_lst 6
    ^ g "  Game "
    ^ g
        (String.concat "    Game "
           (List.map string_of_int (List.init num_games (( + ) 1))))
    ^ g "    Total\n"
  in
  if num_games = 0 then
    "  Scores are calculated at the end of each game.\n\
    \  You need to finish your first game before seeing your score.\n"
  else num_games_row ^ string_of_scores player_lst ^ "\n"

let help_msg =
  g "  Game Commands:\n\n"
  ^ ip ^ "move t to r       Moves the tile(s) [t] to board row [r].\n"
  ^ g "  For Example:\n" ^ ip
  ^ "move 1 4 A3 to B  Moves rack tiles at index 1 and 4, and board \
     row A tile at index 3, to row B.\n" ^ ip
  ^ "undo              Undo most recent move.\n" ^ ip
  ^ "reset             Resets board and rack to start of turn.\n" ^ ip
  ^ "sort color        Sorts rack by tile color.\n" ^ ip
  ^ "sort number       Sorts rack by tile number.\n" ^ ip
  ^ "draw              Draws a new tile and ends turn.\n" ^ ip
  ^ "end turn          Checks for a valid board and ends turn.\n" ^ ip
  ^ "score             Score of previous games.\n" ^ ip
  ^ "help              Display game play commands.\n\n\
    \  The following shortcut commands can be also used:\n\
    \  m, u, r, sc, sn, d, e, s, h.\n\n"

let quit_msg () =
  print_string ("\n" ^ g "  Thank you for playing, goodbye!\n\n");
  Stdlib.exit 0

let get_exception_msg = function
  | AlreadyDrawn s -> already_drawn_msg s
  | AlreadyMoved -> already_moved_msg
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

and handle_end_turn st =
  let curr_p = get_current_player st in
  if curr_p.rack = [] && check_valid st curr_p then (
    let new_st = update_end_game_scores st in
    print_string (win_board new_st (score_msg new_st.players));
    match read_line () with
    | "Y" | "y" | "yes" ->
        play_turn (init_new_round new_st)
          ("  Starting round "
          ^ string_of_int
              (List.length (get_current_player new_st).score + 1)
          ^ ". Enter your command to play.\n")
    | "N" | "n" | "no" | "quit" | "q" -> quit_msg ()
    | _ -> handle_end_turn st)
  else play_turn (end_turn_st st) (end_turn_msg st)

and commands command st =
  try
    match command with
    | Move m -> play_turn (move m st) move_msg
    | Undo -> play_turn (undo_move st) (undo_msg st)
    | Reset -> play_turn (reset_turn st) (reset_msg st)
    | SortByColor -> play_turn (sort_rack_by_color st) sort_col_msg
    | SortByNumber -> play_turn (sort_rack_by_num st) sort_num_msg
    | Draw -> play_turn (draw st) draw_msg
    | EndTurn -> handle_end_turn st
    | Score -> play_turn st (score_msg st.players)
    | Help -> play_turn st help_msg
    | Quit -> quit_msg ()
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
      else if input = "quit" || input = "q" then quit_msg ()
      else welcome st help_msg

let rec random_order_round_draw stack name =
  match read_line () with
  | input ->
      if input = "draw" || input = "d" then (
        let tile = draw_tile stack in
        print_string
          (g "\n  " ^ name ^ " has drawn " ^ string_of_tile 1 tile
         ^ "\n\n");
        (get_tile_number tile, name))
      else if input = "quit" then quit_msg ()
      else (
        print_string
          ("  The command was not entered correctly. Retry.\n" ^ ip);
        random_order_round_draw stack name)

let rec wait_start () =
  print_string ("  Enter 'start' to start the game!\n" ^ ip);
  match read_line () with
  | input ->
      if input = "start" then print_string ""
      else if input = "quit" then quit_msg ()
      else (
        print_string
          ("  The command was not entered correctly. Retry.\n" ^ ip);
        wait_start ())

let rec random_order_round_aux stack acc = function
  | [] ->
      wait_start ();
      acc
  | p :: t ->
      print_string
        ("  " ^ p.name
       ^ ", please enter 'd' or 'draw' to draw a tile.\n" ^ ip);
      let new_acc = random_order_round_draw stack p.name :: acc in
      random_order_round_aux stack new_acc t

let random_order_round st =
  print_string
    (g
       "\n\n\
       \  Starting initial order setting round. Each player will draw \
        a tile.\n\
       \  The player with the highest number will go first.\n\n");
  let order_stack = p_order_tile_stack () in
  let first_p_name =
    random_order_round_aux order_stack [] st.players
    |> List.sort Stdlib.compare
    |> List.rev |> List.hd |> snd
  in
  let first_p =
    List.filter (fun p -> p.name = first_p_name) st.players |> List.hd
  in
  { st with current_turn = first_p.p_number }

let repeat_init_game_aux () =
  print_string ("  Try again or type \"quit\" to exit.\n" ^ ip);
  match read_line () with
  | "quit" | "q" -> quit_msg ()
  | init_game -> init_game

(* [game_start] attempts to initilize state using entered number of
   players and player names. *)
let rec game_start str =
  try
    let state = init_state (parse_start str) |> random_order_round in
    welcome state help_msg
  with
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
