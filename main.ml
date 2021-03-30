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

let tabs = "|\t\t\t\t\t\t\t\t\t\t\t\t\t|\n"

let print_state (st : State.s) : unit =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1

let rec play_turn (st : State.s) : unit = failwith "TODO"

let rec game_start str =
  try play_turn (init_state (parse_start str)) with
  | NameTooLong -> (
      print_string
        (g "\nPlayer names must be shorter than 21 characters.\n");
      print_string ("Try again or type \"quit\" to exit.\n" ^ g "> ");
      match read_line () with
      | "quit" -> Stdlib.exit 0
      | init_game -> game_start init_game)
  | Malformed -> (
      print_string
        (g "\nThe start game command was not entered correctly.\n");
      print_string ("Try again or type \"quit\" to exit.\n" ^ g "> ");
      match read_line () with
      | "quit" -> Stdlib.exit 0
      | init_game -> game_start init_game)

(* [main] is the initial welcome screen and has player input in number
   of players and player names to start a new game. *)
let main () =
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1;
  ANSITerminal.erase Screen;
  print_string
    (" " ^ String.make 103 '_' ^ " \n" ^ rp "" tabs 6 ^ "|\t\t\t\t"
   ^ ig "WELCOME TO THE  " ^ r "R " ^ k "U " ^ o "M " ^ b "M " ^ r "I "
   ^ k "K " ^ o "U " ^ b "B" ^ ig "  GAME" ^ "\t\t\t\t\t|\n"
   ^ rp "" tabs 4 ^ "|"
    ^ g
        "\t\t   Developed by Anthony Coffin-Schmitt, Mina Huh, and \
         Christy Song"
    ^ "\t\t\t|\n|" ^ "\t\t\t\t"
    ^ g "   For CS 3110, Spring 2021"
    ^ "\t\t\t\t\t\t|\n" ^ rp "" tabs 4 ^ "|" ^ String.make 103 '_'
    ^ "|\n\n"
    ^ "  Enter the number of players (2 or 4) and, optionally, the \
       player names. For example:\n" ^ g "  > "
    ^ "4 Clarkson Gries Dijkstra Turing\n\n" ^ g "  > ");
  match read_line () with init_game -> game_start init_game

(* Execute the game engine. *)
let () = main ()
