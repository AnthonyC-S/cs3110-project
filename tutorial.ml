open Tile
open Board
open State
open Textgui

let board_with_all_tiles =
  [
    (Blue, "A");
    (Blue, "B");
    (Orange, "C");
    (Orange, "D");
    (Red, "E");
    (Red, "F");
    (Black, "G");
    (Black, "H");
  ]

let rec mult_add_to_board board row = function
  | [] -> board
  | h :: t -> mult_add_to_board (add_tile h row board) row t

let full_board () =
  let rec aux (acc : b_row list) = function
    | [] -> acc
    | (c, rl) :: t ->
        aux
          (mult_add_to_board acc rl
             (List.init 13 (( + ) 1)
             |> List.map (fun x -> make_t "T" x c)
             |> List.rev))
          t
  in
  let main_tiles = aux [] board_with_all_tiles in
  mult_add_to_board main_tiles "I"
    [ make_t "J" 100 None; make_t "J" 100 None ]

let start_state =
  let blank_state =
    init_state
      [ (1, "Player 1                            Camlkub Tutorial") ]
  in
  let new_player = { (List.hd blank_state.players) with rack = [] } in
  { blank_state with players = [ new_player ] }

let start_tutorial =
  build_board start_state
    (g "  Welcome to the Camlkub Tutorial!"
    ^ "\n\
      \  This tutorial will teach you how to play the game and the \
       commands used.\n\n\
      \  To quit this tutorial at any time, enter \"q\". To continue \
       through the tutorial, press enter.\n")

let tutorial_1 =
  build_board { start_state with board = full_board () } "Full Board!"

let tutorial_pages = [ start_tutorial; tutorial_1 ]
