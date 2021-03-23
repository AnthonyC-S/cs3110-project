open OUnit2
open Tile
open Player
open Board
open State
open Command

let tile_tests = []

let player_tests = []

let board_tests = []

let state_tests = []

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
