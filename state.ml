open Player
open Board

type s = {
  current_turn : int;
  current_board : b;
  past_moves : b list;
  players : p list;
}

let init_state = failwith "TODO"
