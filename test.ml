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
    update_joker_test "Joker is now 1 Red" 1 Red (make_t "J" 0 None)
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

let board = init_board ()

let board2 = add_tile (make_t "T" 1 Red) "B" [] board

let board3 = add_tile (make_t "T" 1 Orange) "B" [] board2

let board4 = add_tile (make_t "T" 1 Blue) "B" [] board3

let board5 = add_tile (make_t "T" 1 Black) "B" [] board4

let board6 = add_tile (make_t "T" 3 Black) "G" [] board5

let add_tile_test
    (name : string)
    (tile : t)
    (row : string)
    (board : b)
    (expected_output : b) =
  name >:: fun _ ->
  assert_equal expected_output (add_tile tile row [] board)

let remove_tile_test
    (name : string)
    (tile : t)
    (row : string)
    (board : b)
    (expected_output : b) =
  name >:: fun _ ->
  assert_equal expected_output (remove_tile tile row [] board)

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
