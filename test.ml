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
    update_joker_test "Joker is now 1 Red" 1 Red (make_t 0 None "J")
      (make_t 1 Red "J");
    update_joker_exception "Not a Joker exception" 1 Red
      (make_t 5 Black "T") Tile.NotAJoker;
  ]

(*****************************************************************)
(* Start of player tests.*)
(*****************************************************************)

let player_tests = []

(*****************************************************************)
(* Start of board tests.*)
(*****************************************************************)
let board_tests = []

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
