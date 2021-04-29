(** Need Player Module Description *)

(** [rack] represents the tile list [t_lst] that the player holds. *)
type rack = Tile.t list

(** [p] represents a player with string name [name], player number
    integer [number]. [played_valid_meld] is a bool showing whether the
    player [p]'s moves in a turn was all valid. [meld_count] is a tile
    list that keeps track of the tiles the [p] moved to make pass the
    initial meld rule. [rack] is the tile list that [p] currently holds.
    [score] is the integer score(s) of [p] games with the last element
    being the most recent game score. [drawn_current_turn] is a bool
    that reflects whether or not [p] drawed a tile in the current turn. *)
type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  meld_count : Tile.t list;
  rack : rack;
  score : int list;
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

(** [player_to_update turn plst] is a player [p] with number [turn] in
    player list [plst]. [p] is the player playing the game currently. *)
val player_to_update : int -> p list -> p

(** [get_current_rack turn plst] is rack [r] of current player [p] with
    number [turn]. *)
val get_current_rack : int -> p list -> rack

(** [meld_sum p] is the sum of all the numbers of the tiles that the
    player [p] used to meet its initial meld condition. *)
val meld_sum : p -> int

(** [check_for_valid_meld p] is true if the sum of the tile numbers
    player [p] used to meet the initial meld condition is more than or
    equal to 30 or is 0. Meld sum of 0 also results in true to account
    for when [p] chose to draw without making any moves. *)
val check_for_valid_meld : p -> bool

(** [update_played_valid_meld p] is player [p'] same as [p] but with
    played_valid_meld field set to true if the initial meld condition
    that [p] didn't meet before is now met. The draw_current_turn field
    is always set to false since this function is called to update [p]'s
    record when [p] ends turn. *)
val update_played_valid_meld : p -> p

(** [add_score turn pl] is an updated player's list with new scores
    added for each player at the end of a game. The winning player in
    the player list [pl], determiend by the current [turn], is awared
    the positive sum of the losing players tile rack sum, with jokers
    counting for 30 points. The losing players get the negative sum of
    the tile numbers for tiles remaining in their own rack, with Jokers
    counting for -30. *)
val add_scores : int -> p list -> p list
