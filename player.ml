open Tile

type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  rack : rack;
  past_racks : rack list;
  score : int;
}

and rack = t list

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
let reset_current_turn_rack p =
  let fst_rack = fst_ele p.past_racks in
  { (empty_past_rack p) with rack = fst_rack }
