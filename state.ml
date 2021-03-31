open Player
open Board
open Tile

type s = {
  current_turn : int;
  current_board : b;
  past_boards : b list;
  players : p list;
  t_stack : Tile.t Stack.t;
}

let init_state player_lst =
  let stack = Tile.make_tile_stack () in
  {
    current_turn = 1;
    current_board = init_board ();
    past_boards = [];
    players = make_players [] stack player_lst;
    t_stack = stack;
  }

exception NotValidIndex

let current_turn st = st.current_turn

let current_board st = st.current_board

let past_boards st = st.past_boards

let players st = st.players

let t_stack st = st.t_stack

let empty_past_boards st = { st with past_boards = [] }

(* Need to update player rack as well.*)
let update_past_boards st =
  { st with past_boards = st.past_boards @ [ st.current_board ] }

(* helper for undo_past_move and reset_current_board*)
let get_fst_elm = function
  | h :: t -> h
  | [] -> failwith "Empty past_boards list"

(* Need to add in player undo, ie move back on rack.*)
let undo_past_move st =
  let last_b = get_fst_elm (List.rev st.past_boards) in
  {
    st with
    current_board = last_b;
    past_boards = List.filter (fun x -> x <> last_b) st.past_boards;
  }

(* Need to also reset player rack*)
let reset_current_board st =
  empty_past_boards
    { st with current_board = get_fst_elm st.past_boards }

let update_current_turn st =
  {
    st with
    current_turn = (st.current_turn mod List.length st.players) + 1;
  }

type result =
  | Legal of s
  | Illegal

(* Need to update past moves and past rack moves. *)
let move_from_rack (st : s) (index : int) (row : string) =
  let rack = get_current_rack st.current_turn st.players in
  let rack_len = List.length rack in
  let tile_to_move = List.filteri (fun i _ -> i = index - 1) rack in
  if rack_len <> 0 && index - 1 < rack_len && index - 1 >= 0 then
    {
      st with
      players = remove_from_rack st.current_turn st.players index;
      current_board =
        add_tile
          (List.find (fun x -> x = x) tile_to_move)
          row [] st.current_board;
    }
  else raise NotValidIndex

let move_from_board = ()

(** helper *)
let rec get_current_player cp (plst : p list) =
  match plst with
  | [] -> []
  | h :: t -> if h.number = cp then h.rack else get_current_player cp t
