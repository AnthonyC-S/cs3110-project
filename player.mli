type rack = Tile.t list

type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  meld_count : Tile.t list;
  rack : rack;
  score : int;
}

exception NotAValidIndex

exception EmptyList

val make_players :
  p list -> Tile.t Stack.t -> (int * string) list -> p list

val add_to_rack : int -> p list -> Tile.t -> p list

val remove_from_rack : int -> int -> p list -> p list

val current_player : int -> p list -> p

val get_current_name : int -> p list -> string

val get_current_meld_status : int -> p list -> bool

val get_current_rack : int -> p list -> rack

val get_current_score : int -> p list -> int

val check_for_valid_meld : p -> bool

val update_played_valid_meld : p -> p

val get_fst_ele : 'a list -> 'a

val remove_fst_ele : 'a list -> 'a list
