open Player
open Board
open Tile
open Command

(** Need State Module Description *)

type s = {
  current_turn : int;
  board : b;
  players : p list;
  t_stack : Tile.t Stack.t;
  past_state : s list;
}

exception HaveNotPlayedMeld

exception InvalidBoardSets

exception InvalidMeld

let init_state player_lst =
  let stack = Tile.make_tile_stack () in
  {
    current_turn = 1;
    board = init_board ();
    players = make_players [] stack player_lst;
    t_stack = stack;
    past_state = [];
  }

let get_current_player st = current_player st.current_turn st.players

let get_other_players st =
  List.filter (fun x -> x <> get_current_player st) st.players

let undo_move st =
  if st.past_state = [] then st
  else
    { (List.hd st.past_state) with past_state = List.tl st.past_state }

let reset_turn st =
  if st.past_state = [] then st
  else { (List.rev st.past_state |> List.hd) with past_state = [] }

let move_from_rack st index row =
  let cur_player = get_current_player st in
  let rack = cur_player.rack in
  let tile_to_move = get_tile_of_index "" index rack in
  let new_player =
    {
      cur_player with
      meld_count = tile_to_move :: cur_player.meld_count;
    }
  in
  {
    st with
    board = add_tile tile_to_move row st.board;
    players =
      new_player :: get_other_players st
      |> remove_from_rack st.current_turn index;
  }

(*Note, had to do the sort list and reverse order so the higher index
  tiles would be moved first and not mess up the index order other
  tiles. *)
let rec multiple_moves_from_rack index_lst row st =
  match List.rev (List.sort compare index_lst) with
  | [] -> { st with board = sort_board_by_num [] st.board }
  | h :: t -> multiple_moves_from_rack t row (move_from_rack st h row)

let move_from_board st from_row index to_row =
  let cur_player = get_current_player st in
  if cur_player.played_valid_meld = false then raise HaveNotPlayedMeld
  else
    let tile =
      get_tile_of_index from_row index
        (List.find (fun { row = x } -> x = from_row) st.board).tiles
    in
    let new_board =
      remove_tile tile from_row st.board |> add_tile tile to_row
    in
    {
      st with
      board = new_board;
      players = cur_player :: get_other_players st;
    }

(* Note, [multiple_moves_from_board] needs to have the board tiles to
   move come in as an association list set up as (string * int) list
   i.e. (from_row * from_index) list. *)
let rec multiple_moves_from_board from_lst to_row st =
  match List.rev (List.sort compare from_lst) with
  | [] -> { st with board = sort_board_by_num [] st.board }
  | (r, i) :: t ->
      multiple_moves_from_board t to_row (move_from_board st r i to_row)

let add_past_state start_turn_state st =
  { st with past_state = start_turn_state :: st.past_state }

let move moves st =
  let start_st = st in
  multiple_moves_from_board moves.from_board moves.to_row st
  |> multiple_moves_from_rack moves.from_rack moves.to_row
  |> add_past_state start_st

(* Note, I did not update the [past_state] list since this will be
   automatically followed by [reset_turn] and then [end_turn] via
   main.ml. *)
let draw st =
  {
    st with
    players =
      add_to_rack st.current_turn st.players (draw_tile st.t_stack);
  }

let sort_rack_by_color st =
  let cur_player = get_current_player st in
  let new_player =
    { cur_player with rack = sort_by_color cur_player.rack }
  in
  { st with players = new_player :: get_other_players st }

let sort_rack_by_num st =
  let cur_player = get_current_player st in
  let new_player =
    { cur_player with rack = sort_by_number cur_player.rack }
  in
  { st with players = new_player :: get_other_players st }

let check_valid_board st = valid_board st.board

let update_current_turn st =
  (st.current_turn mod List.length st.players) + 1

let end_turn st =
  let cur_player = get_current_player st in
  if not (check_valid_board st) then raise InvalidBoardSets
  else if
    (not (check_for_valid_meld cur_player))
    && cur_player.played_valid_meld = false
  then raise InvalidMeld
  else
    {
      st with
      players =
        (cur_player |> update_played_valid_meld) :: get_other_players st;
      current_turn = update_current_turn st;
      past_state = [];
    }
