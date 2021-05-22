open Tile
open Board
open State
open Textgui

let board_with_all_tiles =
  [
    (Blue, "A");
    (Blue, "C");
    (Orange, "G");
    (Orange, "I");
    (Red, "M");
    (Red, "O");
    (Black, "S");
    (Black, "U");
  ]

let rec mult_add_to_board row t_lst board =
  match t_lst with
  | [] -> sort_board_by_num [] board
  | h :: t -> mult_add_to_board row t (add_tile h row board)

let gen_tile_run_lst start len color =
  List.init len (( + ) start)
  |> List.map (fun x -> make_t "T" x color)
  |> List.rev

let gen_tile_group_lst num color_lst =
  List.map (fun x -> make_t "T" num x) color_lst

let tile_lst_of_lst lst =
  List.map
    (fun (i, c) -> make_t (if c = None then "J" else "T") i c)
    lst
  |> List.rev

let start_rack =
  [
    (7, Red);
    (2, Blue);
    (7, Black);
    (100, None);
    (7, Orange);
    (8, Black);
    (13, Red);
    (9, Black);
    (7, Blue);
    (4, Orange);
    (1, Red);
    (10, Blue);
    (2, Red);
    (100, None);
  ]
  |> tile_lst_of_lst

let start_rack_sn = sort_by_number start_rack

let start_rack_sc = sort_by_color start_rack

let full_board =
  let rec aux (acc : b_row list) = function
    | [] -> acc
    | (c, rl) :: t ->
        aux (mult_add_to_board rl (gen_tile_run_lst 1 13 c) acc) t
  in
  let main_tiles = aux (init_board ()) board_with_all_tiles in
  mult_add_to_board "Y"
    [ make_t "J" 100 None; make_t "J" 100 None ]
    main_tiles

let board_with_runs =
  init_board ()
  |> mult_add_to_board "A" (gen_tile_run_lst 6 3 Red)
  |> mult_add_to_board "E" (gen_tile_run_lst 1 6 Blue)
  |> mult_add_to_board "I" (gen_tile_run_lst 10 4 Black)
  |> mult_add_to_board "M" (gen_tile_run_lst 1 13 Orange)

let board_with_groups =
  init_board ()
  |> mult_add_to_board "A" (gen_tile_group_lst 8 [ Orange; Blue; Red ])
  |> mult_add_to_board "E"
       (gen_tile_group_lst 5 [ Orange; Blue; Red; Black ])
  |> mult_add_to_board "I"
       (gen_tile_group_lst 10 [ Orange; Red; Black ])
  |> mult_add_to_board "M"
       (gen_tile_group_lst 1 [ Orange; Blue; Black ])

let board_with_jokers =
  init_board ()
  |> mult_add_to_board "A"
       (tile_lst_of_lst [ (4, Black); (4, Blue); (4, Orange) ]
       @ [ make_t "J" 4 Red ])
  |> mult_add_to_board "E"
       [ make_t "T" 1 Black; make_t "T" 1 Blue; make_t "J" 1 Blue ]
  |> mult_add_to_board "I"
       [ make_t "T" 1 Red; make_t "J" 1 Blue; make_t "J" 1 Orange ]
  |> mult_add_to_board "M"
       [ make_t "T" 4 Black; make_t "J" 5 Black; make_t "T" 6 Black ]
  |> mult_add_to_board "Q"
       [
         make_t "T" 10 Red;
         make_t "T" 11 Red;
         make_t "T" 12 Red;
         make_t "J" 13 Red;
       ]
  |> mult_add_to_board "U"
       [
         make_t "T" 1 Blue;
         make_t "T" 2 Blue;
         make_t "J" 3 Blue;
         make_t "T" 4 Blue;
         make_t "T" 5 Blue;
         make_t "J" 6 Blue;
         make_t "T" 7 Blue;
       ]

let start_state r =
  let blank_state =
    init_state
      [ (1, "Player 1                            Camlkub Tutorial") ]
  in
  let new_player = { (List.hd blank_state.players) with rack = r } in
  { blank_state with players = [ new_player ] }

let start_state_with_rack =
  let blank_state =
    init_state
      [ (1, "Player 1                            Camlkub Tutorial") ]
  in
  let new_player =
    { (List.hd blank_state.players) with rack = start_rack }
  in
  { blank_state with players = [ new_player ] }

let start_tutorial =
  build_board (start_state [])
    (g "  Welcome to the Camlkub Tutorial!"
    ^ "\n\n\
      \  This tutorial will teach you how to play the game and the \
       commands used.\n\
      \  Camlkub is the same as Rummikub, the game is named Camlkub as \
       it was programmed in OCaml. \n\n\
      \  To quit this tutorial at any time, enter \"q\". To continue \
       through the tutorial, press enter.\n" ^ ip)

let tutorial_1 =
  build_board
    { (start_state []) with board = full_board }
    (g "  Camlkub is played with 106 different tiles."
    ^ "\n\n\
      \  There are two sets of tiles numbered 1 to 13 in the colors \
       Blue, Orange, Red, and Black.\n\
      \  There are also two joker tiles that can be any color or number.\n"
    ^ ip)

let tutorial_2 =
  build_board (start_state [])
    (g "  Number of players and play order."
    ^ "\n\n\
      \  There may be 2, 3, or 4 players in the game.\n\n\
      \  Players draw a random tile and the highest tile number \
       determines which player starts.\n\
      \  Other plays than play in the order listed starting after the \
       first player.\n" ^ ip)

let tutorial_3 =
  build_board
    (start_state start_rack)
    (g "  Player Racks"
   ^ "\n\n\
     \  Each player randomly draws 14 tiles from the pile for their \
      rack.\n\
     \  The object of the game is to be the first player to play all \
      the tiles on their rack onto the board.\n\n\
     \  The row above \"Rack:\" titled \"Index:\" allows you to refer \
      to a specific tile for moves.\n\n\
     \  To more easily view your rack, you can sort it by number or \
      color.\n\n\
     \  For example, to sort by number you would enter this command:\n"
   ^ ip ^ "sn ")

let tutorial_4 =
  build_board
    (start_state start_rack_sn)
    (g "  Sorting Racks"
   ^ "\n\n\
     \  The rack is now sorted first by number and then by color.\n\
     \  It is also possible to sort the rack by color using this \
      command:\n" ^ ip ^ "sc ")

let tutorial_5 =
  build_board
    (start_state start_rack_sc)
    (g "  Sorting Racks"
   ^ "\n\n  The rack is now sorted first by color and then by number.\n"
   ^ ip)

let tutorial_6 =
  build_board
    { (start_state []) with board = board_with_runs }
    (g "  Making Sets - Runs"
    ^ "\n\n\
      \  To play tiles to the board, they must form complete (valid) \
       sets.\n\
      \  There are two type of sets: runs and groups.\n\n\
      \  A run is a set of three or more consecutive numbers all in \
       the same color.\n\
      \  The number 1 is always played as the lowest number and 13 can \
       not wrap around back to 1.\n\n\
      \  The board shows examples of valid run sets.\n" ^ ip)

let tutorial_7 =
  build_board
    { (start_state []) with board = board_with_groups }
    (g "  Making Sets - Groups"
    ^ "\n\n\
      \  A group is a set of either three or four tiles of the same \
       number in different colors.\n\n\
      \  The board shows examples of valid group sets.\n" ^ ip)

let tutorial_8 =
  build_board
    { (start_state []) with board = board_with_jokers }
    (g "  Making Sets - Jokers"
    ^ "\n\n\
      \  The joker tile can act as a 'wildcard' to help make sets. \
       There are two jokers in the game. \n\
      \  Each joker can be used as part of a set to help make the set \
       valid.\n\
      \  The joker can be any number and color needed to complete the \
       set.\n\n\
      \  The board shows examples of valid sets, runs and groups, \
       using joker tiles.\n" ^ ip)

let tutorial_pages =
  [
    start_tutorial;
    tutorial_1;
    tutorial_2;
    tutorial_3;
    tutorial_4;
    tutorial_5;
    tutorial_6;
    tutorial_7;
    tutorial_8;
  ]
