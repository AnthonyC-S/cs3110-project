open Tile
open Player
open State
open Command

(* [rp acc str i] concatanates [str] [i] number of times. *)
let rec rp acc str = function
  | 0 -> acc
  | i -> rp (str ^ acc) str (i - 1)

(* black *)
let k s = "\027[38;5;8;1m" ^ s ^ "\027[0m"

(* red *)
let r s = "\027[38;5;9;1m" ^ s ^ "\027[0m"

(* orange *)
let o s = "\027[38;5;208;1m" ^ s ^ "\027[0m"

(* blue *)
let b s = "\027[38;5;033;1m" ^ s ^ "\027[0m"

(* italic green *)
let ig s = "\027[38;5;70;3m" ^ s ^ "\027[0m"

(* regular green *)
let g s = "\027[38;5;70m" ^ s ^ "\027[0m"

(* [ip] is short for input and is frequently used before a read_line. *)
let ip : string = g "  > "

let top_r = " " ^ String.make 103 '_' ^ " \n"

let empty_r = "|\t\t\t\t\t\t\t\t\t\t\t\t\t|\n"

let turn_r cur_player tab_n =
  "|  " ^ g "Current Turn: " ^ cur_player ^ rp "" "\t" tab_n ^ "|\n"

let pile_r pile_size =
  "|     " ^ g "Pile Size: " ^ pile_size ^ rp "" "\t" 11 ^ "|\n"

let top_index_r =
  "| " ^ g "Index:"
  ^ "  1  2  3  4  5  6  7  8  9  10  11  12  13  ||     1  2  3  4  \
     5  6  7  8  9  10  11  12  13   |\n"

let bottom_r = "|" ^ String.make 103 '_' ^ "|\n\n"

let welcome_msg =
  "|\t\t\t\t" ^ ig "  WELCOME TO THE  " ^ r "R " ^ k "U " ^ o "M "
  ^ b "M " ^ r "I " ^ k "K " ^ o "U " ^ b "B" ^ ig "  GAME    "
  ^ "\t\t\t\t|\n"

let developed_by_msg =
  "|"
  ^ g
      "\t\t     Developed by Anthony Coffin-Schmitt, Mina Huh, and \
       Christy Song"
  ^ "\t\t\t|\n|" ^ "\t\t\t\t"
  ^ g "        For CS 3110, Spring 2021"
  ^ "\t\t\t\t\t|\n"

let welcome_board =
  top_r ^ rp "" empty_r 6 ^ welcome_msg ^ rp "" empty_r 4
  ^ developed_by_msg ^ rp "" empty_r 4 ^ bottom_r

let game_commands : string =
  g "  Game commands needed to play:\n\n"
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
  print_string (g "  Thank you for playing, goodbye!\n\n");
  Stdlib.exit 0

let clear_board () =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1

let calc_tabs_needed total_len cur_len =
  (total_len - cur_len + ((total_len - cur_len) mod 8)) / 8

let space_n i =
  if i = 10 || i = 11 || i = 12 || i = 13 then " " else "  "

let convert_n i = string_of_int i ^ space_n i

let convert_tile (tile : Tile.t) =
  match tile with
  | Tile { number = i; color = Blue } -> b (convert_n i)
  | Tile { number = i; color = Orange } -> o (convert_n i)
  | Tile { number = i; color = Red } -> r (convert_n i)
  | Tile { number = i; color = Black } -> k (convert_n i)
  | Tile { number = i; color = None } -> ""
  | Joker _ -> "J  "

let rec string_of_tiles (acc : string) (tiles : Tile.t list) : string =
  match tiles with
  | [] -> acc
  | h :: t -> string_of_tiles (acc ^ convert_tile h) t

let rec string_of_board_rows (acc : string) (board : Board.b_row list) :
    string =
  match board with
  | [] -> acc
  | { row = r1; visible = v1; tiles = t1 } :: t ->
      string_of_board_rows
        (acc ^ "|     " ^ r1 ^ ":  " ^ string_of_tiles "" t1 ^ "\n")
        t

let build_board (st : State.s) : string =
  let cur_player = get_current_name st.current_turn st.players in
  let name_length = String.length cur_player in
  let tabs_for_turn = calc_tabs_needed 107 (name_length + 17) in
  let turn_r = turn_r cur_player tabs_for_turn in
  let pile_size = string_of_int (Stack.length st.t_stack) in
  let pile_r = pile_r pile_size in
  let dash_r = "|" ^ rp "" "-" 103 ^ "|\n" in
  top_r ^ turn_r ^ pile_r ^ empty_r ^ dash_r ^ top_index_r
  ^ string_of_board_rows "" st.current_board
  ^ "\n\n\n"

let rec play_turn (st : State.s) msg : unit =
  clear_board ();
  print_string (build_board st)

let rec welcome (st : State.s) msg : unit =
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
        play_turn st "Enter your command to play."
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
  | Malformed ->
      print_string
        (g "\n  The start game command was not entered correctly.\n");
      game_start (repeat_init_game_aux ())

(* [main] is the initial welcome screen and has player input in number
   of players and player names to start a new game. *)
let main () =
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1;
  ANSITerminal.erase Screen;
  print_string
    (welcome_board
   ^ "  Enter the number of players (2 or 4) and, optionally, the \
      player names. For example:\n" ^ ip
   ^ "4 Clarkson Gries Dijkstra Turing\n" ^ ip);
  match read_line () with init_game -> game_start init_game

(* Execute the game engine. *)
let () = main ()
