(* type s *)

(*new*)
type s = {
  current_turn : int;
  current_board : Board.b;
  past_boards : Board.b list;
  players : Player.p list;
  t_stack : Tile.t Stack.t;
}

val init_state : (int * string) list -> s

val current_turn : s -> int

val current_board : s -> Board.b

val past_boards : s -> Board.b list

val players : s -> Player.p list

val t_stack : s -> Tile.t Stack.t

val empty_past_boards : s -> s

val update_past_boards : s -> s

val undo_past_move : s -> s

val move_from_rack : s -> int -> string -> s

val get_current_player : int -> Player.p list -> Tile.t list
