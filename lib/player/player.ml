open Tile

type rack = Tile.t list

type p = {
  name : string;
  p_number : int;
  played_valid_meld : bool;
  meld_count : t list;
  rack : rack;
  score : int list;
  drawn_current_turn : bool;
}

let rec make_players acc stack = function
  | [] -> List.rev acc
  | (p_number, name) :: t ->
      make_players
        ({
           name;
           p_number;
           played_valid_meld = false;
           meld_count = [];
           rack = make_tile_rack stack;
           score = [];
           drawn_current_turn = false;
         }
        :: acc)
        stack t

let player_to_update turn player_lst =
  List.find (fun { p_number = x; _ } -> x = turn) player_lst

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

let get_current_rack turn player_lst =
  (player_to_update turn player_lst).rack

let meld_sum player =
  List.fold_left ( + ) 0 (numbers_of_t [] player.meld_count)

let check_for_valid_meld player =
  meld_sum player >= 30 || meld_sum player = 0

let update_played_valid_meld player =
  if (not player.played_valid_meld) && meld_sum player >= 30 then
    { player with played_valid_meld = true; drawn_current_turn = false }
  else { player with drawn_current_turn = false }

(** [get_score_from_rack t_lst] is the negative integer score for a
    losing player at the end of the game. Score is calculated by summing
    their remaining tile rack [t_lst]. Jokers are each worth -30 points
    tiles are worth their negative tile number. *)
let get_score_from_rack t_lst =
  let rec aux acc = function
    | Joker _ :: tail -> aux (acc - 30) tail
    | Tile h :: tail -> aux (acc - h.number) tail
    | [] -> acc
  in
  aux 0 t_lst

(** [get scores losing_players] is an updated player list with the
    scores of the [losing_players] at the end of a game. *)
let get_scores losing_players =
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
        aux
          ({ h with score = get_score_from_rack h.rack :: h.score }
          :: acc)
          t
  in
  aux [] losing_players

let add_scores turn player_lst =
  let winner = player_to_update turn player_lst in
  let losing_players = List.filter (fun x -> x <> winner) player_lst in
  let losing_players_new_scores = get_scores losing_players in
  let winning_score =
    List.map (fun x -> -List.hd x.score) losing_players_new_scores
    |> List.fold_left ( + ) 0
  in
  { winner with score = winning_score :: winner.score }
  :: losing_players_new_scores

let compare_player_number x y = Stdlib.compare x.p_number y.p_number
