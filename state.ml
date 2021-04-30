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

exception AlreadyDrawn of string

exception AlreadyMoved

(* Spec is in signature. *)
let init_state player_lst =
  let stack = make_tile_stack () in
  let plst = make_players [] stack player_lst in
  {
    current_turn = (List.hd plst).number;
    board = init_board ();
    players = plst;
    t_stack = stack;
    past_state = [];
  }

let rec reset_players_turns st = function
  | [] -> st
  | (newn, name) :: t ->
      let new_ps = reset_player_turn st.players name newn in
      let new_st = { st with players = new_ps } in
      reset_players_turns new_st t

let get_current_player st = current_player st.current_turn st.players

(** [get_other_player st] is the player list of the other 1 or 3 players
    who are not currently in their turn in the state [st]. *)
let get_other_players st =
  List.filter (fun x -> x <> get_current_player st) st.players

(* Spec is in signature. *)
let undo_move st =
  if (get_current_player st).drawn_current_turn then
    raise (AlreadyDrawn "undo")
  else if st.past_state = [] then st
  else
    { (List.hd st.past_state) with past_state = List.tl st.past_state }

(* Spec is in signature. *)
let reset_turn st =
  if (get_current_player st).drawn_current_turn then
    raise (AlreadyDrawn "reset")
  else if st.past_state = [] then st
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

(* Spec is in signature. *)
let move moves st =
  if (get_current_player st).drawn_current_turn then
    raise (AlreadyDrawn "move after drawn")
  else
    let start_st = st in
    multiple_moves_from_board moves.from_board moves.to_row st
    |> multiple_moves_from_rack moves.from_rack moves.to_row
    |> add_past_state start_st

(* Spec is in signature. *)
let draw st =
  if (get_current_player st).drawn_current_turn then
    raise (AlreadyDrawn "draw again")
  else if st.past_state = [] then
    {
      st with
      players =
        add_to_rack st.current_turn st.players (draw_tile st.t_stack);
    }
  else raise AlreadyMoved

(* Spec is in signature. *)
let sort_rack_by_color st =
  let cur_player = get_current_player st in
  let new_player =
    { cur_player with rack = sort_by_color cur_player.rack }
  in
  { st with players = new_player :: get_other_players st }

(* Spec is in signature. *)
let sort_rack_by_num st =
  let cur_player = get_current_player st in
  let new_player =
    { cur_player with rack = sort_by_number cur_player.rack }
  in
  { st with players = new_player :: get_other_players st }

let check_valid_board st = valid_board st.board

let update_current_turn st =
  (st.current_turn mod List.length st.players) + 1

let check_valid st cp =
  if not (check_valid_board st) then raise InvalidBoardSets
  else if check_for_valid_meld cp || cp.played_valid_meld then true
  else raise InvalidMeld

let end_turn_new_st st =
  let cur_player = get_current_player st in
  {
    st with
    players =
      (cur_player |> update_played_valid_meld) :: get_other_players st;
    current_turn = update_current_turn st;
    past_state = [];
  }

(* and other_ps = get_other_players st in { st with players = other_ps @
   [ cur_player |> update_played_valid_meld ]; current_turn = (List.hd
   other_ps).number; past_state = []; } *)
