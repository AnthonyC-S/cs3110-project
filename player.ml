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

let empty_past_rack player = { player with past_racks = [] }

let update_past_rack player =
  { player with past_racks = player.past_racks @ [ player.rack ] }

exception EmptyRack

(* helper *)
let last_ele_lst_rest lst =
  match List.rev lst with
  | [] -> raise EmptyRack
  | h :: t -> (h, List.rev t)

(* could use nth, but suggested to not use nth? *)

let undo_past_rack player =
  let last_ele_rest = last_ele_lst_rest player.past_racks in
  {
    player with
    rack = fst last_ele_rest;
    past_racks = snd last_ele_rest;
  }

(* helper *)
let fst_ele = function [] -> raise EmptyRack | h :: t -> h

let reset_current_turn_rack player =
  let fst_rack = fst_ele player.past_racks in
  { (empty_past_rack player) with rack = fst_rack }
