type tile =
  | Tile of t
  | Joker of t

(* [t] is a helper type to define a tile record.*)
and t = {
  number : int;
  color : color;
}

and color =
  | Blue
  | Orange
  | Red
  | Black
  | None

exception NotEnoughTiles

let n_lst = List.init 13 (( + ) 1)

let c_lst = [ Blue; Orange; Red; Black ]

let joker = Joker { number = 0; color = None }

let make_tile_lst () =
  let rec make_pile_aux acc n_lst c =
    match n_lst with
    | [] -> acc
    | n :: ns ->
        make_pile_aux (Tile { number = n; color = c } :: acc) ns c
  in
  joker :: joker
  :: List.concat_map (make_pile_aux [] n_lst) (c_lst @ c_lst)

(* Note, this helper function was modified from here:
   https://stackoverflow.com/a/15095713 *)
let shuffle_tile_lst (acc : tile list) =
  Random.self_init ();
  let random_int_pile = List.map (fun x -> (Random.int 1000, x)) acc in
  let sorted_int_pile = List.sort compare random_int_pile in
  List.map snd sorted_int_pile

let make_tile_stack () =
  make_tile_lst () |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

let draw_tile (stack : tile Stack.t) =
  try Stack.pop stack with Stack.Empty -> raise NotEnoughTiles

let make_tile_rack stack =
  if Stack.length stack >= 14 then
    let rec make_rack_aux stack acc = function
      | 0 -> acc
      | i -> make_rack_aux stack (draw_tile stack :: acc) (i - 1)
    in
    make_rack_aux stack [] 14
  else raise NotEnoughTiles
