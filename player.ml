open Tile

type rack = Tile.t list

type p = {
  name : string;
  number : int;
  played_valid_meld : bool;
  meld_count : t list;
  rack : rack;
  score : int;
  drawn_current_turn : bool;
}

exception EmptyList

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

(** [remove_fst_ele lst] is [lst] with the first element of [lst]
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

let add_to_rack turn player_lst tile =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack = update_player.rack @ [ tile ];
    drawn_current_turn = true;
  }
  :: List.filter (fun x -> x <> update_player) player_lst

let remove_from_rack turn index player_lst =
  let update_player = player_to_update turn player_lst in
  {
    update_player with
    rack = List.filteri (fun i _ -> i <> index - 1) update_player.rack;
  }
  :: List.filter (fun x -> x <> update_player) player_lst

let current_player turn player_lst =
  List.find (fun { number = x } -> x = turn) player_lst

let get_current_rack turn player_lst =
  (current_player turn player_lst).rack

let meld_sum player =
  List.fold_left ( + ) 0 (numbers_of_t [] player.meld_count)

let check_for_valid_meld (player : p) : bool =
  meld_sum player >= 30 || meld_sum player = 0

let update_played_valid_meld player : p =
  if (not player.played_valid_meld) && meld_sum player >= 30 then
    { player with played_valid_meld = true; drawn_current_turn = false }
  else { player with drawn_current_turn = false }

let calculate_rack_score t_lst =
  let rec get_score acc = function
    | Joker t1 :: Joker t2 :: tail -> get_score (acc - 60) tail
    | Joker t1 :: tail -> get_score (acc - 30) tail
    | Tile h :: tail -> get_score (acc - h.number) tail
    | [] -> acc
  in
  get_score 0 t_lst

(* *)
let add_end_game_scores turn player_lst = failwith "TODO"
