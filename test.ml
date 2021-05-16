open OUnit2
open Tile
open Player
open Board
open State
open Command

(***************************************************************** Test
  Plan - Need to Add

  Test Plan Rubric [4 points] The test plan should be located in a
  comment at the top of the test file.

  -4: The test plan is missing. -1: The test plan does not explain which
  parts of the system were automatically tested by OUnit vs. manually
  tested. -1: The test plan does not explain what modules were tested by
  OUnit and how test cases were developed (black box, glass box,
  randomized, etc.). -1: The test plan does not provide an argument for
  why the testing approach demonstrates the correctness of the system.
  *****************************************************************)

(*****************************************************************)
(* Test Helper Functions                                         *)
(*****************************************************************)
let new_players_lst = [ (1, "A"); (2, "B") ]

(* Makes a tile list/rack consisting of Red 1, Black 13 .. Black 1*)
let new_starting_rack =
  List.init 13 (( + ) 1)
  |> List.map (fun x -> make_t "T" x Black)
  |> List.rev
  |> List.cons (make_t "T" 1 Red)

let staring_rack_sorted_color =
  (List.init 13 (( + ) 1) |> List.map (fun x -> make_t "T" x Black))
  @ [ make_t "T" 1 Red ]

let staring_rack_sorted_number =
  List.init 12 (( + ) 2)
  |> List.map (fun x -> make_t "T" x Black)
  |> List.cons (make_t "T" 1 Red)
  |> List.cons (make_t "T" 1 Black)

let new_player_rec =
  {
    name = "A";
    p_number = 1;
    played_valid_meld = false;
    meld_count = [];
    rack = new_starting_rack;
    score = [];
    drawn_current_turn = false;
  }

let new_test_state () =
  let stack = make_ordered_tile_stack () in
  let plst = make_players [] stack new_players_lst in
  {
    current_turn = 1;
    board = init_board ();
    players = plst;
    t_stack = stack;
    past_state = [];
  }

(* this helper state has a Red 1 on row A and played_valid_meld set to
   true for player 1.*)
let valid_meld_state () =
  let st = new_test_state () in
  let player_one = List.hd st.players in
  let new_st =
    {
      st with
      players =
        { player_one with played_valid_meld = true }
        :: List.tl st.players;
    }
  in
  move { from_board = []; from_rack = [ 1 ]; to_row = "A" } new_st

(*****************************************************************)
(* Start of Tile Module Tests                                    *)
(*****************************************************************)

let update_joker_test name n c t expected_output =
  name >:: fun _ -> assert_equal expected_output (update_joker n c t)

let update_joker_exception name n c t expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> update_joker n c t)

let make_t_exception name t n c expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> make_t t n c)

let stack_size_test name stack expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (tile_stack_size stack)
    ~printer:string_of_int

let draw_tile_exception name stack expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> draw_tile stack)

let make_rack_exception name stack expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> make_tile_rack stack)

let new_tile_stack = make_tile_stack ()

let draw_one_tile_stack =
  let s = make_tile_stack () in
  ignore (draw_tile s);
  s

let tile_tests =
  [
    update_joker_test "Joker is now 1 Red" 1 Red (make_t "J" 0 None)
      (make_t "J" 1 Red);
    update_joker_exception "Not a Joker exception" 0 Red
      (make_t "J" 100 None) NotAJoker;
    update_joker_exception "Not a Joker exception" 14 Red
      (make_t "J" 100 None) NotAJoker;
    update_joker_exception "Not a Joker exception" 1 Red
      (make_t "T" 5 Black) NotAJoker;
    make_t_exception "Not a Tile or Joker" "C" 10 Red InvalidTile;
    stack_size_test "Size of new tile stack is 106" new_tile_stack 106;
    stack_size_test "Draw one tile, stack size is 105"
      draw_one_tile_stack 105;
    draw_tile_exception
      "Empty stack raises NotEnoughTiles when drawing tile"
      (let s = make_tile_stack () in
       Stack.clear s;
       s)
      NotEnoughTiles;
    stack_size_test "Making a tile rack removes 14 tiles from stack"
      (let s = make_tile_stack () in
       ignore (make_tile_rack s);
       s)
      92;
    make_rack_exception
      "Empty stack raises NotEnoughTiles when making rack"
      (let s = make_tile_stack () in
       Stack.clear s;
       s)
      NotEnoughTiles;
  ]

let stack_size_test name stack expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (tile_stack_size stack)

(*****************************************************************)
(* Start of Player Module Tests                                  *)
(*****************************************************************)
let make_players_test name acc stack players_lst expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (List.hd (make_players acc stack players_lst))

let player_tests =
  [
    make_players_test "player one's record" []
      (make_ordered_tile_stack ())
      new_players_lst new_player_rec;
  ]

(*****************************************************************)
(* Start of Board Module Tests                                   *)
(*****************************************************************)

let board = init_board ()

let board2 = add_tile (make_t "T" 1 Red) "B" board

let board3 = add_tile (make_t "T" 1 Orange) "B" board2

let board4 = add_tile (make_t "T" 1 Blue) "B" board3

let board5 = add_tile (make_t "T" 1 Black) "B" board4

let board6 = add_tile (make_t "T" 3 Black) "G" board5

let add_tile_test
    (name : string)
    (tile : t)
    (row : string)
    (board : b)
    (expected_output : b) =
  name >:: fun _ ->
  assert_equal expected_output (add_tile tile row board)

let remove_tile_test
    (name : string)
    (tile : t)
    (row : string)
    (board : b)
    (expected_output : b) =
  name >:: fun _ ->
  assert_equal expected_output (remove_tile tile row board)

(*Testing for validity of groups and runs*)
let valid_board_test
    (name : string)
    (board : b)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (valid_board board)

(*Testing adding/removing tiles from board*)

let board_tests =
  [
    add_tile_test "Add Red 1 tile to empty board" (make_t "T" 1 Red) "B"
      board board2;
    remove_tile_test "Remove Red 1 tile" (make_t "T" 1 Red) "B" board2
      board;
    valid_board_test "Valid board with one group" board5 true;
    valid_board_test "Empty board" board true;
    valid_board_test "One valid group, one invalid" board6 false;
  ]

(*****************************************************************)
(* Start of State Module Tests                                   *)
(*****************************************************************)
let get_current_player_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (get_current_player st)

let move_from_rack_test_check_rack name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (List.hd (move moves st).players).rack

let move_from_rack_test_check_board name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (List.hd (move moves st).board)

let move_from_board_meld_not_met_test name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> move moves st)

let move_from_board_test_check_from_row name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (List.hd (move moves st).board)

let move_from_board_test_check_to_row name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (List.nth (move moves st).board 1)

let move_after_draw_test name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> move moves)

let draw_after_move_test name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> move moves st |> draw)

let draw_after_draw_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> draw)

let undo_move_test name st expected_output =
  name >:: fun _ -> OUnit2.assert_equal expected_output (undo_move st)

let undo_move_already_drawn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> undo_move)

let reset_turn_test name st expected_output =
  name >:: fun _ -> OUnit2.assert_equal expected_output (reset_turn st)

let reset_turn_already_drawn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> reset_turn)

let sort_rack_by_color_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (sort_rack_by_color st)

let sort_rack_by_num_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (sort_rack_by_num st)

let state_tests =
  [
    get_current_player_test "current player should be A"
      (new_test_state ())
      (List.hd (new_test_state ()).players);
    move_from_rack_test_check_rack "rack should be missing Red 1"
      { from_board = []; from_rack = [ 1 ]; to_row = "A" }
      (new_test_state ())
      (List.tl new_starting_rack);
    move_from_rack_test_check_board "board row A should have Red 1"
      { from_board = []; from_rack = [ 1 ]; to_row = "A" }
      (new_test_state ())
      { row = "A"; tiles = [ make_t "T" 1 Red ] };
    move_from_board_meld_not_met_test
      "exception raised for move from board with meld not met"
      { from_board = [ ("A", 1) ]; from_rack = []; to_row = "B" }
      (new_test_state ()) HaveNotPlayedMeld;
    move_from_board_test_check_from_row "board row A should be empty"
      { from_board = [ ("A", 1) ]; from_rack = []; to_row = "B" }
      (valid_meld_state ())
      { row = "A"; tiles = [] };
    move_from_board_test_check_to_row "board row B should have Red 1"
      { from_board = [ ("A", 1) ]; from_rack = []; to_row = "B" }
      (valid_meld_state ())
      { row = "B"; tiles = [ make_t "T" 1 Red ] };
    move_after_draw_test
      "raises exception for moving after already drawing"
      { from_board = []; from_rack = [ 1 ]; to_row = "A" }
      (new_test_state ()) (AlreadyDrawn "move after drawn");
    draw_after_move_test
      "raises exception for drawing after already moving"
      { from_board = []; from_rack = [ 1 ]; to_row = "A" }
      (new_test_state ()) AlreadyMoved;
    draw_after_draw_test
      "raises exception for drawing after already drawing"
      (new_test_state ()) (AlreadyDrawn "draw again");
    undo_move_test "undo move of state with no moves"
      (new_test_state ()) (new_test_state ());
    undo_move_test "undo move of state with one move"
      (move
         { from_board = []; from_rack = [ 1 ]; to_row = "A" }
         (new_test_state ()))
      (new_test_state ());
    undo_move_already_drawn_test
      "undo move for player with drawn_current_turn = true"
      (new_test_state ()) (AlreadyDrawn "undo");
    reset_turn_test "reset turn of state with one move"
      (move
         { from_board = []; from_rack = [ 1 ]; to_row = "A" }
         (new_test_state ()))
      (new_test_state ());
    reset_turn_test "reset turn of state with no moves"
      (new_test_state ()) (new_test_state ());
    reset_turn_already_drawn_test
      "reset turn for player with drawn_current_turn = true"
      (new_test_state ()) (AlreadyDrawn "reset");
    (let player_lst = (new_test_state ()).players in
     sort_rack_by_color_test "sorts rack by color" (new_test_state ())
       {
         (new_test_state ()) with
         players =
           {
             (List.hd player_lst) with
             rack = staring_rack_sorted_color;
           }
           :: List.tl player_lst;
       });
    (let player_lst = (new_test_state ()).players in
     sort_rack_by_num_test "sorts rack by number" (new_test_state ())
       {
         (new_test_state ()) with
         players =
           {
             (List.hd player_lst) with
             rack = staring_rack_sorted_number;
           }
           :: List.tl player_lst;
       });
  ]

(*****************************************************************)
(* Start of Command Module Tests                                 *)
(*****************************************************************)
let command_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           tile_tests;
           player_tests;
           board_tests;
           state_tests;
           (*command_tests; *)
         ]

let _ = run_test_tt_main suite
