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
