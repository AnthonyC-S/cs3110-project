open Tile
open Player
open State
open Command

let print_blue =
  ANSITerminal.print_string [ ANSITerminal.blue; ANSITerminal.on_white ]

(*black on white background *)
let bk =
  ANSITerminal.print_string
    [ ANSITerminal.black; ANSITerminal.on_white ]

let on_white = ANSITerminal.print_string [ ANSITerminal.on_white ]

(* bold red*)
let br s = print_string ("\027[31;1m" ^ s ^ "\027[0m")

(* bold yellow*)
let by s = print_string ("\027[33;1m" ^ s ^ "\027[0m")

(* bold blue*)
let bb s = print_string ("\027[34;1m" ^ s ^ "\027[0m")

(* green *)
let bg s = print_string ("\027[32;1m" ^ s ^ "\027[0m")

let play_game file_name = failwith "TODO"

let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 105 45;
  ANSITerminal.set_cursor 1 1;
  print_string " ";
  print_string (String.make 103 '_');
  print_string " ";
  bg "\n\n  Welcome to ";
  br "R";
  print_string " ";
  bk "U";
  print_string " ";
  by "M";
  print_string " ";
  bb "M";
  print_string " ";
  br "I";
  print_string " ";
  bk "K";
  print_string " ";
  by "U";
  print_string " ";
  bb "B";
  print_string " ";
  bg "!";

  print_string
    "\n\n\
    \  Enter the number of players (2 or 4) and the player names.\n\
    \  For example: ";
  bg "> ";
  print_string "4 Clarkson Gries Dijkstra Turing\n\n";
  bg "  > ";
  match read_line () with
  | exception End_of_file -> ()
  | init_game -> play_game init_game

(* Execute the game engine. *)
let () = main ()
