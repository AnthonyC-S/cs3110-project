open Tile

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
  meld_count : t list;
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
let rec make_players acc stack = function
  | [] -> List.rev acc
  | (number, name) :: t ->
      make_players
        ({
           name;
           number;
           played_valid_meld = false;
           meld_count = [];
           rack = make_tile_rack stack;
           score = 0;
           drawn_current_turn = false;
         }
        :: acc)
        stack t

(** [get_fst_ele lst] is the first element of [lst]. It raises the
    [EmptyList] exception if [lst] is empty. *)
let get_fst_ele = function [] -> raise EmptyList | h :: t -> h

(** [remove_fst_elm lst] is [lst] with the first element of [lst]
    removed or an empty list if [lst] has one element. Raises
    [EmptyList] if [lst] is empty.*)
let remove_fst_ele = function
  | [] -> raise EmptyList
  | [ h ] -> []
  | h :: t -> t

(** [player_to_update turn player_lst] is player [p] in [player_lst]
    that has the player numer [turn]. *)
let player_to_update turn player_lst =
  List.find (fun { number = x } -> x = turn) player_lst

(** [add_to_rack turn player_lst tile] is player list [player_lst'] with
    the new record of player [p] with number [turn] consed onto and the
    old record of [p] deleted. The new record of the player [p] has tile
    [t] inside its rack field and drawn_current_turn field set to true.
    This is called when the player with the number [turn] draws a tile. *)
let add_to_rack turn player_lst tile =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack = update_player.rack @ [ tile ];
    drawn_current_turn = true;
  }
  :: List.filter (fun x -> x <> update_player) player_lst

(** [remove_from_rack turn i p_lst] is player list [p_lst'] with the new
    record of player [p] with number [turn] consed onto and the old
    record of [p] deleted. The new record of [p] has a tile in [p]'s
    rack with index number [i - 1] removed to match the 0-based
    indexing. This is called when the player moves a tile to the board. *)
let remove_from_rack turn index player_lst =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack = List.filteri (fun i _ -> i <> index - 1) update_player.rack;
  }
  :: List.filter (fun x -> x <> update_player) player_lst

(* Helper for the get_current_... functions below. *)
let current_player turn player_lst =
  List.find (fun { number = x } -> x = turn) player_lst

let get_current_name turn player_lst =
  (current_player turn player_lst).name

let get_current_meld_status turn player_lst =
  (current_player turn player_lst).played_valid_meld

let get_current_rack turn player_lst =
  (current_player turn player_lst).rack

let get_current_score turn player_lst =
  (current_player turn player_lst).score

let meld_sum player =
  List.fold_left ( + ) 0 (numbers_of_t [] player.meld_count)

let check_for_valid_meld (player : p) : bool =
  meld_sum player >= 30 || meld_sum player = 0

(* Note, requires that the board is valid and this is called at
   [end_turn]. *)
let update_played_valid_meld player : p =
  if (not player.played_valid_meld) && meld_sum player >= 30 then
    { player with played_valid_meld = true }
  else player
