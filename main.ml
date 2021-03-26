open Tile
open Player
open State
open Command

let print_red = ANSITerminal.print_string [ ANSITerminal.red ]

let print_green = ANSITerminal.print_string [ ANSITerminal.green ]

let print_on_blue = ANSITerminal.print_string [ ANSITerminal.on_blue ]

let print_on_yellow =
  ANSITerminal.print_string [ ANSITerminal.on_yellow ]

let print_on_red = ANSITerminal.print_string [ ANSITerminal.on_red ]

let print_on_black = ANSITerminal.print_string [ ANSITerminal.on_black ]

(* let extract = function | Tile.Tile t -> t | Tile.Joker t -> t | _ ->
   failwith "Unknown" *)

let play_game file_name = print_endline "Playing Game"

let main () =
  print_string "\n\nWelcome to the game ";
  print_on_blue "R";
  print_on_yellow "U";
  print_on_red "M";
  print_on_black "M";
  print_on_blue "I";
  print_on_yellow "K";
  print_on_red "U";
  print_on_black "B";
  print_on_blue "!";

  print_endline
    "Please enter the name of the game file you want to load.";
  print_green "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
