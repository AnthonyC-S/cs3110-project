open OUnit2
open Tile
open Player
open Board
open State
open Command

(*****************************************************************)
(* Start of tile tests.*)
(*****************************************************************)

let update_joker_test name n c t expected_output =
  name >:: fun _ -> assert_equal expected_output (update_joker n c t)

let update_joker_exception name n c t expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> update_joker n c t)

let tile_tests =
  [
    update_joker_test "Joker is now 1 Red" 1 Red joker
      (make_t "J" 1 Red);
    update_joker_exception "Not a Joker exception" 1 Red
      (make_t "T" 5 Black) Tile.NotAJoker;
  ]

(*****************************************************************)
(* Start of player tests.*)
(*****************************************************************)

let player_tests = []

(*****************************************************************)
(* Start of board tests.*)
(*****************************************************************)

let validjokergroup = [(make_t "T" 1 Red);(make_t "T" 1 Blue); (make_t "J" 1 Black); (make_t "T" 1 Orange)]
let validjokerrun = [(make_t "T" 5 Red);(make_t "J" 6 Red); (make_t "T" 7 Red); (make_t "T" 8 Red)]
let invalidgroup = [(make_t "T" 2 Red);(make_t "T" 2 Black); (make_t "T" 2 Blue); (make_t "T" 2 Black)]
let invalidrun = [(make_t "T" 1 Red);(make_t "T" 2 Red); (make_t "T" 3 Red); (make_t "T" 5 Red)]
let board = init_board()
let board2 = add_tile (make_t "T" 1 Red) "B" [] board
let board3 = add_tile (make_t "T" 1 Orange) "B" [] board2

let board4 = add_tile (make_t "T" 1 Blue) "B" [] board3
let board5 = add_tile (make_t "T" 1 Black) "B" [] board4
let board6 = add_tile (make_t "T" 3 Black) "G" [] board5



(*Testing for validity of groups and runs*)
let valid_group_test (name : string) (lst : t list) (expected_output : bool) = 
  name >:: fun _ -> assert_equal expected_output (valid_group lst)

let valid_run_test (name : string) (lst : t list) (expected_output : bool) = 
  name >:: fun _ -> assert_equal expected_output (valid_run lst)

let valid_board_test (name : string) (board : b) (expected_output : bool) = 
  name >:: fun _ -> assert_equal expected_output (valid_board true board)

(*Testing adding/removing tiles from board*)

let board_tests = [
  valid_group_test "Valid group with a joker" validjokergroup true;
  valid_run_test "Valid run with a joker" validjokerrun true;
  valid_group_test "Invalid group" invalidgroup false;
  valid_run_test "Invalid run" invalidrun false;
  valid_board_test "Valid board with one group" board5 true;
  valid_board_test "Empty board" board true;
  valid_board_test "One valid group, one invalid" board6 false;

]

(*****************************************************************)
(* Start of state tests.*)
(*****************************************************************)
let state_tests = []

(*****************************************************************)
(* Start of command tests.*)
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
           command_tests;
         ]

let _ = run_test_tt_main suite
