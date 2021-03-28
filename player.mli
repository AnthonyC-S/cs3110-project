(* type p type rack = Tile.t list *)

(*new*)
type rack = Tile.t list

type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  rack : rack;
  past_racks : rack list;
  score : int;
}

val make_players :
  p list -> Tile.t Stack.t -> (int * string) list -> p list

val empty_past_rack : p -> p

val update_past_rack : p -> p

val undo_past_rack : p -> p

val reset_current_turn_rack : p -> p

val add_to_rack : int -> p list -> Tile.t -> p list

val remove_from_rack : int -> p list -> int -> p list

val get_current_name : int -> p list -> string

val get_current_meld_status : int -> p list -> bool

val get_current_rack : int -> p list -> rack

val get_past_racks : int -> p list -> rack list

val get_current_score : int -> p list -> int
