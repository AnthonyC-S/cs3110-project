open Tile
open Board
open State
open Textgui
open Command

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
       (gen_tile_run_lst 10 3 Red @ [ make_t "J" 13 Red ])
  |> mult_add_to_board "U"
       (gen_tile_run_lst 1 2 Blue
       @ [ make_t "J" 3 Blue ]
       @ gen_tile_run_lst 4 2 Blue
       @ [ make_t "J" 6 Blue; make_t "T" 7 Blue ])

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
    (h "  Welcome to the Camlkub Tutorial!  "
    ^ "\n\n\
      \  This tutorial will teach you how to play the game and the \
       commands used.\n\n\
      \  Camlkub is the same as Rummikub, the game is named Camlkub as \
       it was programmed in OCaml. \n\n\
      \  To quit this tutorial at any time, enter " ^ c "q"
    ^ ". To continue through the tutorial, press " ^ c "enter" ^ ".\n"
    ^ ip)

let tutorial_1 =
  build_board
    { (start_state []) with board = full_board }
    (h "  Camlkub is played with 106 different tiles.  "
    ^ "\n\n\
      \  There are two sets of tiles numbered 1 to 13 in the colors "
    ^ bw "Blue" ^ ", " ^ ow "Orange" ^ ", " ^ rw "Red" ^ ", and "
    ^ kw "Black"
    ^ ".\n\n\
      \  There are also two joker tiles that can be any color or number.\n"
    ^ ip)

let tutorial_2 =
  build_board (start_state [])
    (h "  Number of players and play order.  "
    ^ "\n\n\
      \  There may be 2, 3, or 4 players in the game.\n\n\
      \  Players draw a random tile and the highest tile number \
       determines which player starts.\n\n\
      \  Other players then play in the order listed, starting after \
       the first player.\n" ^ ip)

let tutorial_3 =
  build_board
    (start_state start_rack)
    (h "  Player Racks  "
   ^ "\n\n\
     \  Each player randomly draws 14 tiles from the pile for their \
      rack.\n\n\
     \  The object of the game is to be the first player to play all \
      the tiles on their rack onto the board.\n\n\
     \  The row above \"Rack:\" titled \"Index:\" allows you to refer \
      to a specific tile for moves.\n\n\
     \  To more easily view your rack, you can sort it by number or \
      color.\n\n\
     \  For example, to sort by number you would enter this command:\n"
   ^ ip ^ i "sn ")

let tutorial_4 =
  build_board
    (start_state start_rack_sn)
    (h "  Sorting Racks  "
   ^ "\n\n\
     \  The rack is now sorted first by number and then by color.\n\n\
     \  It is also possible to sort the rack by color using this \
      command:\n" ^ ip ^ i "sc ")

let tutorial_5 =
  build_board
    (start_state start_rack_sc)
    (h "  Sorting Racks  "
   ^ "\n\n  The rack is now sorted first by color and then by number.\n"
   ^ ip)

let tutorial_6 =
  build_board
    { (start_state []) with board = board_with_runs }
    (h "  Making Sets - Runs  "
    ^ "\n\n\
      \  To play tiles to the board, they must form complete (valid) \
       sets.\n\n\
      \  There are two type of sets: runs and groups.\n\n\
      \  A run is a set of three or more consecutive numbers all in \
       the same color.\n\n\
      \  The number 1 is always played as the lowest number and 13 can \
       not wrap around back to 1.\n\n\
      \  The board shows examples of valid run sets.\n" ^ ip)

let tutorial_7 =
  build_board
    { (start_state []) with board = board_with_groups }
    (h "  Making Sets - Groups  "
    ^ "\n\n\
      \  A group is a set of either three or four tiles of the same \
       number in different colors.\n\n\
      \  The board shows examples of valid group sets.\n" ^ ip)

let tutorial_8 =
  build_board
    { (start_state []) with board = board_with_jokers }
    (h "  Making Sets - Jokers  "
    ^ "\n\n\
      \  The joker tile can act as a 'wildcard' to help make sets. \
       There are two jokers in the game. \n\n\
      \  Each joker can be used as part of a set to help make the set \
       valid.\n\n\
      \  The joker can be any number and color needed to complete the \
       set.\n\n\
      \  The board shows examples of valid sets, runs and groups, \
       using joker tiles.\n" ^ ip)

let tutorial_9 =
  build_board
    (start_state start_rack)
    (h "  Start of the Game - Meld  "
    ^ "\n\n\
      \  The Meld field on the board states whether the current player \
       has met the meld count. \n\n\
      \  Until Meld is met, players are unable to rearrange tiles on \
       the board or use Joker tiles. \n\n\
      \  To meet the meld count, a player must place tiles on the \
       board so that the sum of the \n\
      \  tile numbers is greater than or equal to 30. \n\n\
      \  In the game above, Player 1 can meet the meld count by first \
       entering:\n" ^ ip ^ i "move 6 10 14 to A ")

let st_10 =
  move
    { from_board = []; from_rack = [ 6; 10; 14 ]; to_row = "A" }
    (start_state start_rack)

let tutorial_10 =
  build_board st_10
    (h "  Start of the Game - Meld  "
    ^ "\n\n\
      \  This brings the meld count to 21.\n\n\
      \  Players can use multiple commands to reach the count of 30 as \
       long as it's within one turn. \n\n\
      \  Player 1 can next enter: \n" ^ ip ^ i "move 10 6 8 to B ")

let st_11 =
  end_turn_st
    (move
       { from_board = []; from_rack = [ 10; 6; 8 ]; to_row = "B" }
       st_10)

let tutorial_11 =
  build_board st_11
    (h "  Start of the Game - Meld  "
    ^ "\n\n\
      \  This brings the meld count to 45, well above the required 30.\n\n\
      \  After ending turn, Player 1's Meld status will change to \
       \"Met\" as seen above.\n\n\
      \  Starting from their next turn, Player 1 can use Joker tiles \
       and move tiles on the board. \n" ^ ip)

let tutorial_12 =
  build_board st_11
    (h "  Player Actions  "
   ^ "\n\n\
     \  During a turn, a player can either move tiles onto the board, \
      or rearrange tiles on\n\
     \  the board to make space for their own tiles before entering \
      endturn. \n\n\
     \  Before ending a turn, all tiles on the board must be either a \
      valid run or group.\n\n\
     \  If neither option is possible, the player must draw a tile \
      from the pile and endturn.\n" ^ ip)

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
    tutorial_9;
    tutorial_10;
    tutorial_11;
    tutorial_12;
  ]
