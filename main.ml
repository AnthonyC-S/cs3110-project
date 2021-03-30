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
let ig s = "\027[38;5;82;3m" ^ s ^ "\027[0m"

let g s = "\027[38;5;82m" ^ s ^ "\027[0m"

let tabs = "|\t\t\t\t\t\t\t\t\t\t\t\t\t|\n"

let print_state (st : State.s) : unit =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1

let rec play_turn (st : State.s) : unit = failwith "TODO"

let rec game_start str =
  try play_turn (init_state (parse_start str))
  with Malformed -> (
    print_string (g "\nThere was a problem starting your game. \n");
    print_endline
      "Are you sure you entered the start game command in the correct \
       format? \n\
       try again or type \"quit\" to exit.";
    print_string (g "> ");
    match read_line () with
    | exception End_of_file -> raise Malformed
    | "quit" -> Stdlib.exit 0
    | init_game -> game_start init_game)

(* [main] is the initial welcome screen and has player input in number
   of players and player names to start a new game. *)
let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1;
  print_string
    (" " ^ String.make 103 '_' ^ " " ^ rp "" tabs 6 ^ "|\t\t\t\t"
   ^ ig "WELCOME TO THE  " ^ r "R " ^ k "U " ^ o "M " ^ b "M " ^ r "I "
   ^ k "K " ^ o "U " ^ b "B" ^ ig "  GAME" ^ "\t\t\t\t\t|"
   ^ rp "" tabs 4 ^ "|"
    ^ g
        "\t\t   Developed by Anthony Coffin-Schmitt, Mina Huh, and \
         Christy Song"
    ^ "\t\t\t\t\t|\n|" ^ "\t\t\t\t"
    ^ g "   For CS 3110, Spring 2021"
    ^ "\t\t\t\t\t\t|\n" ^ rp "" tabs 4 ^ "|" ^ String.make 103 '_'
    ^ "|\n\n"
    ^ "  Enter the number of players (2 or 4) and, optionally, the \
       player names. For example:\n" ^ g "  > "
    ^ "4 Clarkson Gries Dijkstra Turing\n\n" ^ g "  > ");
  match read_line () with
  | exception End_of_file -> ()
  | init_game -> game_start init_game

(* Execute the game engine. *)
let () = main ()
