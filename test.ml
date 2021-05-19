open OUnit2
open Tile
open Player
open Board
open State
open Command

(********************************************************************
  Testing Plan:

  The following units were automatically tested by unit testing / bisect
  testing: Tile, Player, Board, State, and Command. Bisect coverage
  greater than 90% was the achieved in these modules. 100% coverage was
  not possible as some pattern matching cases should be impossible to
  reach based on specifications - those matching cases were included to
  allow the pattern match to be exhaustive.

  All unit test cases were created using glass box methods with the goal
  of covering edge cases. Higher order modules/functions were
  prioritized as they would call several or many sub-functions and
  therefore, be tested as well. For example, almost every function in
  the State module was unit tested directly, and thus tested many of the
  sub-functions that the State module called. This allowed high bisected
  coverage without testing every smaller and simple function in the core
  Modules, such as Tile, Player and Board.

  The Main module and Textgui were tested through playing the game and
  testing for edge cases and using Utop and viewing the resulting state
  type. The Main module was mostly not conducive to unit testing as many
  of its functions returned the unit type. All three group members
  contributed to testing and trying to discover unusual game setups that
  could cause any errors.

  While this testing strategy cannot guarantee complete correctness of
  the system, it does meet the standards needed for an initial release.
  A game such as Rummikub can generate an extremely high number of tile
  combinations on the board and it would be nearly impossible to test
  all cases. However, many of these cases can be broken into smaller
  groups of similar structure and we aimed to tested against these more
  generalized groups.

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

(* Helpers Used in Tile Tests *)
let new_tile_stack = make_tile_stack ()

let draw_one_tile_stack =
  let s = make_tile_stack () in
  ignore (draw_tile s);
  s

(* Helpers used in State Tests *)
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

(* State with Black 1 on row A and played_valid_meld set to true for
   player 1.*)
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
  move { from_board = []; from_rack = [ 14 ]; to_row = "A" } new_st

(* Helpers used in Player Tests*)
let player_lst_with_jokers =
  [
    {
      name = "A";
      p_number = 1;
      played_valid_meld = false;
      meld_count = [];
      rack = [];
      score = [];
      drawn_current_turn = false;
    };
    {
      name = "B";
      p_number = 2;
      played_valid_meld = false;
      meld_count = [];
      rack = [ make_t "J" 100 None; make_t "J" 100 None ];
      score = [];
      drawn_current_turn = false;
    };
  ]

let update_valid_meld_player =
  List.hd
    (new_test_state ()
    |> move
         { from_board = []; from_rack = [ 2; 3; 4; 5 ]; to_row = "A" })
      .players

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

let p_order_tile_stack_size_test name expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (tile_stack_size (p_order_tile_stack ()))
    ~printer:string_of_int

let draw_tile_exception name stack expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> draw_tile stack)

let make_rack_exception name stack expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () -> make_tile_rack stack)

let get_tile_num_test name tile expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (get_tile_number tile)

let sort_by_num_test name tile_lst expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (sort_by_number tile_lst)

let get_tile_of_index_exception name row index tile_lst expected_output
    =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun () ->
      get_tile_of_index row index tile_lst)

let tile_tests =
  [
    update_joker_test "joker is now 1 Red" 1 Red (make_t "J" 0 None)
      (make_t "J" 1 Red);
    update_joker_exception "not a Joker exception" 0 Red
      (make_t "J" 100 None) NotAJoker;
    update_joker_exception "not a Joker exception" 14 Red
      (make_t "J" 100 None) NotAJoker;
    update_joker_exception "not a Joker exception" 1 Red
      (make_t "T" 5 Black) NotAJoker;
    make_t_exception "not a Tile or Joker" "C" 10 Red InvalidTile;
    stack_size_test "size of new tile stack is 106" new_tile_stack 106;
    stack_size_test "draw one tile, stack size is 105"
      draw_one_tile_stack 105;
    draw_tile_exception
      "empty stack raises NotEnoughTiles when drawing tile"
      (let s = make_tile_stack () in
       Stack.clear s;
       s)
      NotEnoughTiles;
    stack_size_test "making a tile rack removes 14 tiles from stack"
      (let s = make_tile_stack () in
       ignore (make_tile_rack s);
       s)
      92;
    p_order_tile_stack_size_test
      "draw stack for determining order has a size of 9" 9;
    make_rack_exception
      "empty stack raises NotEnoughTiles when making rack"
      (let s = make_tile_stack () in
       Stack.clear s;
       s)
      NotEnoughTiles;
    get_tile_num_test "checks tile number of a joker"
      (make_t "J" 100 None) 100;
    sort_by_num_test "checks if joker is correctly sorted"
      [ make_t "T" 2 Orange; make_t "J" 2 Blue ]
      [ make_t "J" 2 Blue; make_t "T" 2 Orange ];
    get_tile_of_index_exception "raises InvalidIndex exn" "A" 2
      [ make_t "T" 2 Orange ]
      (InvalidIndex ("A", 2));
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

let get_current_rack_test name turn player_lst expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (get_current_rack turn player_lst)

let add_scores_test name player_lst (expected_output : int) =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (List.hd
       (List.find (fun x -> x.name = "B") (add_scores 1 player_lst))
         .score)

let update_played_valid_meld_test name player expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (update_played_valid_meld player).played_valid_meld

let player_tests =
  [
    make_players_test "player one's record" []
      (make_ordered_tile_stack ())
      new_players_lst new_player_rec;
    get_current_rack_test "player one's rack" 1
      (new_test_state ()).players new_starting_rack;
    add_scores_test "checks that jokers are scored correctly"
      player_lst_with_jokers (-60);
    update_played_valid_meld_test
      "updates a player after playing valid meld"
      update_valid_meld_player true;
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

let board7 = add_tile (make_t "T" 2 Red) "B" board2

let board8 = add_tile (make_t "J" 100 None) "B" board3

let add_tile_test name tile row board expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (List.find (fun x -> x.row = row) (add_tile tile row board))

let remove_tile_test name tile row board expected_output =
  name >:: fun _ ->
  assert_equal expected_output (remove_tile tile row board)

let valid_board_test name board expected_output =
  name >:: fun _ -> assert_equal expected_output (valid_board board)

let board_tests =
  [
    add_tile_test "Add Red 1 tile to empty board" (make_t "T" 1 Red) "B"
      board
      { row = "B"; tiles = [ Tile { number = 1; color = Red } ] };
    add_tile_test "add joker to Red 1 and Red 2" (make_t "J" 100 None)
      "B" board7
      {
        row = "B";
        tiles =
          [
            Joker { number = 3; color = Red };
            Tile { number = 2; color = Red };
            Tile { number = 1; color = Red };
          ];
      };
    add_tile_test "add two jokers to Red 1 and Red 2"
      (make_t "J" 100 None) "B"
      (add_tile (make_t "J" 100 None) "B" board7)
      {
        row = "B";
        tiles =
          [
            Joker { number = 3; color = Red };
            Joker { number = 4; color = Red };
            Tile { number = 2; color = Red };
            Tile { number = 1; color = Red };
          ];
      };
    add_tile_test "add joker to empty board" (make_t "J" 100 None) "B"
      board
      { row = "B"; tiles = [ Joker { number = 100; color = None } ] };
    add_tile_test "add joker to a group of 2" (make_t "J" 100 None) "B"
      board3
      {
        row = "B";
        tiles =
          [
            Joker { number = 1; color = Blue };
            Tile { number = 1; color = Red };
            Tile { number = 1; color = Orange };
          ];
      };
    add_tile_test "add two jokers to a group of 2" (make_t "J" 100 None)
      "B" board8
      {
        row = "B";
        tiles =
          [
            Joker { number = 1; color = Black };
            Joker { number = 1; color = Blue };
            Tile { number = 1; color = Red };
            Tile { number = 1; color = Orange };
          ];
      };
    add_tile_test "add two jokers empty board" (make_t "J" 100 None) "B"
      (add_tile (make_t "J" 100 None) "B" board)
      {
        row = "B";
        tiles =
          [
            Joker { number = 100; color = None };
            Joker { number = 100; color = None };
          ];
      };
    remove_tile_test "Remove Red 1 tile" (make_t "T" 1 Red) "B" board2
      board;
    valid_board_test "Valid board with one group" board5 true;
    valid_board_test "Empty board" board true;
    valid_board_test "One valid group, one invalid" board6 false;
  ]

(*****************************************************************)
(* Start of State Module Tests                                   *)
(*****************************************************************)

(* To reduce code duplication, the State module function being tested is
   a parameter. *)
let general_state_function_test name func st expected_output =
  name >:: fun _ -> OUnit2.assert_equal expected_output (func st)

let init_state_test name player_lst expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (Stack.length (init_state player_lst).t_stack)

let init_new_round_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (List.hd
       (List.find (fun x -> x.name = "A") (init_new_round st).players)
         .score)

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

let move_row_full_exn_test name moves_1 st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> move moves_1 st)

let draw_after_move_test name moves st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> move moves st |> draw)

let draw_after_draw_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> draw)

let undo_move_already_drawn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> undo_move)

let reset_turn_already_drawn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ -> draw st |> reset_turn)

let check_valid_test name st cp expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (check_valid st cp)

let check_valid_raises_exn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ ->
      check_valid (get_current_player st) st)

let end_turn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (end_turn_st st).current_turn

let end_turn_raises_exn_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_raises expected_output (fun _ ->
      (end_turn_st st).current_turn)

let update_end_game_scores_test name st expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output
    (List.hd (List.hd (update_end_game_scores st).players).score)

let state_tests =
  [
    init_state_test "init new state with 4 players, checks stack size"
      [ (1, "A"); (2, "B"); (3, "C"); (4, "D") ]
      50;
    general_state_function_test "current player should be A"
      get_current_player (new_test_state ())
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
    move_from_board_test_check_to_row "board row B should have Black 1"
      { from_board = [ ("A", 1) ]; from_rack = []; to_row = "B" }
      (valid_meld_state ())
      { row = "B"; tiles = [ make_t "T" 1 Black ] };
    move_row_full_exn_test "move to a row that already has 13 tiles exn"
      {
        from_board = [];
        from_rack = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ];
        to_row = "B";
      }
      (new_test_state ()) (RowAlreadyFull "B");
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
    general_state_function_test "undo move of state with no moves"
      undo_move (new_test_state ()) (new_test_state ());
    general_state_function_test "undo move of state with one move"
      undo_move
      (move
         { from_board = []; from_rack = [ 1 ]; to_row = "A" }
         (new_test_state ()))
      (new_test_state ());
    undo_move_already_drawn_test
      "undo move for player with drawn_current_turn = true"
      (new_test_state ()) (AlreadyDrawn "undo");
    general_state_function_test "reset turn of state with one move"
      reset_turn
      (move
         { from_board = []; from_rack = [ 1 ]; to_row = "A" }
         (new_test_state ()))
      (new_test_state ());
    general_state_function_test "reset turn of state with no moves"
      reset_turn (new_test_state ()) (new_test_state ());
    reset_turn_already_drawn_test
      "reset turn for player with drawn_current_turn = true"
      (new_test_state ()) (AlreadyDrawn "reset");
    (let player_lst = (new_test_state ()).players in
     general_state_function_test "sorts rack by color"
       sort_rack_by_color (new_test_state ())
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
     general_state_function_test "sorts rack by number" sort_rack_by_num
       (new_test_state ())
       {
         (new_test_state ()) with
         players =
           {
             (List.hd player_lst) with
             rack = staring_rack_sorted_number;
           }
           :: List.tl player_lst;
       });
    (let cp = get_current_player (new_test_state ()) in
     check_valid_test "check valid board on empty board" cp
       (new_test_state ()) true);
    (let new_st =
       new_test_state ()
       |> move
            { from_board = []; from_rack = [ 2; 3; 4 ]; to_row = "A" }
     in
     let cp = get_current_player new_st in
     check_valid_test "check valid board on after meeting meld > 30" cp
       (new_test_state ()
       |> move
            { from_board = []; from_rack = [ 2; 3; 4 ]; to_row = "A" })
       true);
    (let cp = get_current_player (valid_meld_state ()) in
     check_valid_test
       "check valid board after legal moves and previously met meld" cp
       (valid_meld_state ()
       |> move { from_board = []; from_rack = [ 12; 13 ]; to_row = "A" }
       )
       true);
    check_valid_raises_exn_test
      "check valid board after one invalid move"
      (new_test_state ()
      |> move { from_board = []; from_rack = [ 1 ]; to_row = "A" })
      InvalidBoardSets;
    check_valid_raises_exn_test
      "check valid board after valid moves but meld not met"
      (new_test_state ()
      |> move
           { from_board = []; from_rack = [ 12; 13; 14 ]; to_row = "A" }
      )
      InvalidMeld;
    end_turn_test "end turn after drawing" (new_test_state () |> draw) 2;
    end_turn_test "end turn without drawing" (new_test_state ()) 1;
    end_turn_raises_exn_test "end turn with invalid board sets"
      (new_test_state ()
      |> move { from_board = []; from_rack = [ 1 ]; to_row = "A" })
      InvalidBoardSets;
    update_end_game_scores_test "correct score for player one"
      (new_test_state ()) 93;
    init_new_round_test "start new round, score is carried over"
      (new_test_state () |> update_end_game_scores)
      93;
  ]

(*****************************************************************)
(* Start of Command Module Tests                                 *)
(*****************************************************************)

let parse_start_test name str expected_output =
  name >:: fun _ ->
  OUnit2.assert_equal expected_output (parse_start str)

let parse_start_exception_test name str exc =
  name >:: fun _ -> (fun () -> parse_start str) |> assert_raises exc

let command_tests =
  [
    parse_start_test "2 players, 2 names" "2   a   B "
      [ (1, "a"); (2, "B") ];
    parse_start_test "2 players, no names" "2 "
      [ (1, "Player 1"); (2, "Player 2") ];
    parse_start_test "2 players, 1 name" "2 one"
      [ (1, "one"); (2, "Player 2") ];
    parse_start_exception_test "2 players, 4 name -> malformed"
      "2 one 2 3 4" Malformed;
    parse_start_exception_test "two players,  - name too long"
      "two   afljksljfklajfkdlsjfk  " NameTooLong;
    parse_start_exception_test "3 players, 3 names - name too long"
      "3   B B " NotUniqueNames;
    parse_start_test "3 players, 1 name" "THREE first "
      [ (1, "first"); (2, "Player 2"); (3, "Player 3") ];
    parse_start_test "3 players, 2 names" "3 a b "
      [ (1, "a"); (2, "b"); (3, "Player 3") ];
    parse_start_test "3 players, 3 names" "three a b c "
      [ (1, "a"); (2, "b"); (3, "c") ];
    parse_start_test "3 players, no names" "three "
      [ (1, "Player 1"); (2, "Player 2"); (3, "Player 3") ];
    parse_start_exception_test "3 players, 4 name -> malformed"
      "3 one 2 3 4" Malformed;
    parse_start_test "4 players, 1 name" "4 first "
      [
        (1, "first"); (2, "Player 2"); (3, "Player 3"); (4, "Player 4");
      ];
    parse_start_test "4 players, 2 names" "four a b "
      [ (1, "a"); (2, "b"); (3, "Player 3"); (4, "Player 4") ];
    parse_start_test "4 players, 3 names" "FOUR a b c "
      [ (1, "a"); (2, "b"); (3, "c"); (4, "Player 4") ];
    parse_start_test "4 players, 4 names" "FOUR a b c D"
      [ (1, "a"); (2, "b"); (3, "c"); (4, "D") ];
    parse_start_test "4 players, no names" "4 "
      [
        (1, "Player 1");
        (2, "Player 2");
        (3, "Player 3");
        (4, "Player 4");
      ];
    parse_start_exception_test "4 players, 5 name -> malformed"
      "4 one 2 3 4 MALFORMED" Malformed;
    parse_start_exception_test "malformed START Command" "trhee"
      Malformed;
  ]

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
