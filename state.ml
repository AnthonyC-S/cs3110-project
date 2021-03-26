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
