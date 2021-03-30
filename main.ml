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

let rec play_turn (st : State.s) msg : unit =
  clear_board ();
  print_string "Now in play_turn"

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
      if input = "play" then play_turn st "Enter your command to play."
      else if input = "quit" then quit_game ()
      else welcome st game_commands

let repeat_init_game_aux () =
  print_string ("  Try again or type \"quit\" to exit.\n" ^ ip);
  match read_line () with
  | "quit" -> quit_game ()
  | init_game -> init_game

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
