type s = {
  current_turn : int;
  current_board : Board.b;
  players : Player.p list;
  t_stack : Tile.t Stack.t;
  past_state : s list;
}

exception HaveNotPlayedMeld

exception InvalidBoardSets

exception InvalidMeld

val init_state : (int * string) list -> s

val undo_move : s -> s

val reset_turn : s -> s

val move_from_rack : s -> int -> string -> s

val multiple_moves_from_rack : int list -> string -> s -> s

val assign_joker_in_rack : s -> int -> Tile.color -> int -> s

val move_from_board : s -> string -> int -> string -> s

val multiple_moves_from_board : (string * int) list -> string -> s -> s

val move : Command.move_phrase -> s -> s

val draw : s -> s

val move_from_rack : s -> int -> string -> s

val get_current_player : s -> Player.p

val sort_rack_by_color : s -> s

val sort_rack_by_num : s -> s

val end_turn : s -> s
