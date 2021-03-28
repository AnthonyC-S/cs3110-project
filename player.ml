open Tile

type rack = Tile.t list

type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
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

(* Helper for [add_to_rack] and [remove_from_rack]. *)
let player_to_update turn player_lst =
  List.find (fun { number = x } -> x = turn) player_lst

(* Called from State.ml when player draws a new tile. *)
let add_to_rack turn player_lst tile =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack = List.sort compare (tile :: update_player.rack);
  }
  :: List.filter (fun x -> x <> update_player) player_lst

let remove_from_rack turn player_lst index =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack =
      List.sort compare
        (List.filteri (fun i _ -> i <> index - 1) update_player.rack);
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
