type s = {
  current_turn : int;
  current_board : Board.b;
  players : Player.p list;
  past_boards : Board.b list;
  t_stack : Tile.t Stack.t;
}

exception NotValidIndex

exception HaveNotPlayedMeld

exception InvalidBoardSets

val init_state : (int * string) list -> s

val undo_move : s -> s

val reset_turn : s -> s

val move_from_rack : s -> int -> string -> s

val multiple_moves_from_rack : s -> int list -> string -> s

val assign_joker_in_rack : s -> int -> Tile.color -> int -> s

val move_from_board : s -> string -> int -> string -> s

val multiple_moves_from_board : s -> (int * string) list -> string -> s

val draw : s -> s

val end_turn : s -> s
