open Player
open Board
open Tile

type s = {
  current_turn : int;
  current_board : b;
  past_moves : b list;
  players : p list;
  t_stack : Tile.t Stack.t;
}

let init_state player_lst =
  let stack = Tile.make_tile_stack () in
  {
    current_turn = 1;
    current_board = Board.init_board ();
    past_moves = [];
    players = Player.make_players [] stack player_lst;
    t_stack = stack;
  }

let current_turn st = st.current_turn

let current_board st = st.current_board

let past_moves st = st.past_moves

let players st = st.players

let t_stack st = st.t_stack

let empty_past_moves st = { st with past_moves = [] }

let update_past_moves st =
  { st with past_moves = st.past_moves @ [ st.current_board ] }

(* helper for undo_past_move and reset_current_board*)
let get_fst_elm = function
  | h :: t -> h
  | [] -> failwith "Empty past_moves list"

let undo_past_move st =
  let last_b = get_fst_elm (List.rev st.past_moves) in
  {
    st with
    current_board = last_b;
    past_moves = List.filter (fun x -> x <> last_b) st.past_moves;
  }

let reset_current_board st =
  empty_past_moves { st with current_board = get_fst_elm st.past_moves }

let update_current_turn st =
  {
    st with
    current_turn = (st.current_turn mod List.length st.players) + 1;
  }
