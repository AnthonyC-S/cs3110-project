type t =
  | Tile of tile
  | Joker of tile

and tile = {
  number : int;
  color : color;
}

and color =
  | Blue
  | Orange
  | Red
  | Black
  | None

let n_lst = List.init 13 (( + ) 1)

let c_lst = [ Blue; Orange; Red; Black; Blue; Orange; Red; Black ]

let rec make_pile_aux pile n_lst c =
  match n_lst with
  | [] -> pile
  | n :: ns ->
      make_pile_aux (Tile { number = n; color = c } :: pile) ns c

let make_pile () =
  Joker { number = 0; color = None }
  :: Joker { number = 0; color = None }
  :: List.concat_map (make_pile_aux [] n_lst) c_lst

(* Note, this function was found here:
   https://stackoverflow.com/a/15095713 *)
let shuffle_pile (pile : t list) =
  Random.self_init ();
  let random_int_pile = List.map (fun x -> (Random.int 1000, x)) pile in
  let sorted_int_pile = List.sort compare random_int_pile in
  List.map snd sorted_int_pile

let stack_pile =
  make_pile () |> shuffle_pile |> List.to_seq |> Stack.of_seq

let draw_tile =
  try Stack.pop stack_pile with Stack.Empty -> failwith "Empty Stack"

(* let deal_rack = *)
