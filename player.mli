(** Need Player Module Description *)

(** [rack] represents the tile list [t_lst] that the player holds. *)
type rack = Tile.t list

(** [p] represents a player with string name [name], player number
    integer [number]. [played_valid_meld] is a bool showing whether the
    player [p]'s moves in a turn was all valid. [meld_count] is a tile
    list that keeps track of the tiles the [p] moved to make pass the
    initial meld rule. [rack] is the tile list that [p] currently holds.
    [score] is the current integer score of [p]. [drawn_current_turn] is
    a bool that reflects whether or not [p] drawed a tile in the current
    turn. *)
type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  meld_count : Tile.t list;
  rack : rack;
  score : int;
  drawn_current_turn : bool;
}

(** The exception [EmptyList] is raised when a list being operated on is
    empty. *)
exception EmptyList

(** [make_players acc stack pinfo_lst] is player list [p_lst] with each
    player number and name from (number, name) associative list
    [pinfo_lst]. The players in [p_lst] are ordered the same as in
    [pinfo_lst]. The rack of each player is drawn from [stack]. Require:
    Input is a player association list in the format
    [(player_num *
   player_name);..]. Example:
    [(1, "Clarkson"); (2, "Lee")]. *)
val make_players :
  p list -> Tile.t Stack.t -> (int * string) list -> p list

(** [add_to_rack turn player_lst tile] is player list [player_lst'] with
    the new record of player [p] with number [turn] consed onto and the
    old record of [p] deleted. The new record of the player [p] has tile
    [t] inside its rack field and drawn_current_turn field set to true.
    This is called when the player with the number [turn] draws a tile. *)
val add_to_rack : int -> p list -> Tile.t -> p list

(** [remove_from_rack turn i p_lst] is player list [p_lst'] with the new
    record of player [p] with number [turn] consed onto and the old
    record of [p] deleted. The new record of [p] has a tile in [p]'s
    rack with index number [i - 1] removed to match the 0-based
    indexing. This is called when the player moves a tile to the board. *)
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
