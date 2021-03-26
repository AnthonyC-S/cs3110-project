open Player
open Board
open Tile

type s = {
  current_turn : int;
  stack : Tile.t Stack.t;
  current_board : b;
  past_moves : b list;
  players : p list;
}

(* let init_state player_lst = { current_turn = 1; stack =
   Tile.make_tile_stack (); current_board = Board.init_board ();
   past_moves = []; players = Player.make_players [] stack player_lst; } *)
