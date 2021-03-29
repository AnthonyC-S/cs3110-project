open Tile

type rack = Tile.t list

type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  meld_count : t list;
  past_meld_count : t list list;
  rack : rack;
  past_racks : rack list;
  score : int;
}

exception NotAValidIndex

(* Input is a player association list in the format [(player_num *
   player_name);..]. Example: [(1, "Clarkson"); (2, "Lee")]. *)
let rec make_players acc stack = function
  | [] -> List.rev acc
  | (number, name) :: t ->
      make_players
        ({
           name;
           number;
           played_valid_meld = false;
           meld_count = [];
           past_meld_count = [];
           rack = make_tile_rack stack;
           past_racks = [];
           score = 0;
         }
        :: acc)
        stack t

(** [empty_past_rack p] is player [p] with the past_racks field set to
    an empty list of racks. *)
let empty_past_rack p = { p with past_racks = [] }

(** [update_past_rack p] is player [p] with the current rack [r] of [p]
    appended at the end of past_racks list. *)
let update_past_rack p =
  { p with past_racks = p.past_racks @ [ p.rack ] }

exception EmptyRack

(** [last_ele_lst_rest lst] is a pair with the first element being the
    last element of [lst] and the second element being the rest of
    [lst]. *)
let last_ele_lst_rest lst =
  match List.rev lst with
  | [] -> raise EmptyRack
  | h :: t -> (h, List.rev t)

(* could use nth, but suggested to not use nth?? *)

(** [undo_past_rack p] is player [p] with rack in [p] set to the last
    element of its past_racks which is the most recent configuration of
    rack. The last element is then removed from the past_racks of [p]. *)
let undo_past_rack p =
  let last_ele_rest = last_ele_lst_rest p.past_racks in
  { p with rack = fst last_ele_rest; past_racks = snd last_ele_rest }

(** [fst_ele lst] is the first element of [lst]. It raises the
    EmptryRack exception if [lst] is empty. SHOULD I JUST USE LIST.HD?? *)
let fst_ele = function [] -> raise EmptyRack | h :: t -> h

(** [reset_current_turn_rack p] is player [p] with rack configuration
    set to the initial configuration from the start of its turn and the
    past_rack emptied out. *)
let reset_current_turn_rack player =
  let fst_rack = fst_ele player.past_racks in
  { (empty_past_rack player) with rack = fst_rack }

(* Helper for [add_to_rack] and [remove_from_rack]. *)
let player_to_update turn player_lst =
  List.find (fun { number = x } -> x = turn) player_lst

(* Called from State.ml when player draws a new tile. *)
let add_to_rack turn player_lst tile =
  let update_player = player_to_update turn player_lst in
  { update_player with rack = tile :: update_player.rack }
  :: List.filter (fun x -> x <> update_player) player_lst

let remove_from_rack turn index player_lst =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack = List.filteri (fun i _ -> i <> index - 1) update_player.rack;
  }
  :: List.filter (fun x -> x <> update_player) player_lst

(* Need to implement a recursive function that can remove multiple tiles
   from the rack in one call. *)

(* Helper for the get_current_... functions below. *)
let current_player turn player_lst =
  List.find (fun { number = x } -> x = turn) player_lst

let get_current_name turn player_lst =
  (current_player turn player_lst).name

let get_current_meld_status turn player_lst =
  (current_player turn player_lst).played_valid_meld

let get_current_rack turn player_lst =
  (current_player turn player_lst).rack

let get_past_racks turn player_lst =
  (current_player turn player_lst).past_racks

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
