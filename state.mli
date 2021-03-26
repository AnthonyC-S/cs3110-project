type s

val init_state : (int * string) list -> s

val current_turn : s -> int

val current_board : s -> Board.b

val past_moves : s -> Board.b list

val players : s -> Player.p list

val t_stack : s -> Tile.t Stack.t

val empty_past_moves : s -> s

val update_past_moves : s -> s

val undo_past_move : s -> s
