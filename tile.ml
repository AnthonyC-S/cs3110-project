type color =
  | Blue
  | Orange
  | Red
  | Black
  | None

(* [t_rec] is a helper for tile type and defines a tile record.*)
type t_rec = {
  number : int;
  color : color;
}

type t =
  | Tile of t_rec
  | Joker of t_rec

exception NotEnoughTiles

exception NotAJoker

exception InvalidTile

(** [n_lst] is a list of the numbers used in Rummikub, 1..13. *)
let n_lst = List.init 13 (( + ) 1)

(** [c_lst] is a list of the four color types used in Rummikub and None
    to represent unassigned Jokers. *)
let c_lst = [ Blue; Orange; Red; Black ]

let make_t n c t =
  match t with
  | "T" -> Tile { number = n; color = c }
  | "J" -> Joker { number = n; color = c }
  | _ -> failwith "Not a Tile or Joker"

(** [joker] is tile representing an unassigned Joker. *)
let joker = make_t 0 None "J"

let update_joker n c = function
  | Joker t -> make_t n c "J"
  | Tile t -> raise NotAJoker

(*[make_tile_aux acc n_lst c] is a helper to [make_tile_lst]. [acc] is
  the accumulator, [n_lst] is list 0..13, and [c] is the current color.*)
let rec make_tile_aux acc n_lst c =
  match n_lst with
  | [] -> acc
  | n :: ns -> make_tile_aux (make_t n c "T" :: acc) ns c

(** [make_tile_lst] is the 106 ordered tiles representing a full
    Rummikub pile. Ordered as with two jokers at the head and then sets
    of numbers 1..13 in the following color order: Blue, Orange, Red,
    Black, Blue, Orange, Red, Black. *)
let make_tile_lst () =
  joker :: joker
  :: List.concat_map (make_tile_aux [] n_lst) (c_lst @ c_lst)

(** [shuffle_tile_lst tl] is the randomly ordered tiles from [tl] the
    tile list. Note, this helper function was modified from here:
    https://stackoverflow.com/a/15095713. *)
let shuffle_tile_lst (acc : t list) =
  Random.self_init ();
  let random_int_lst = List.map (fun x -> (Random.int 1000, x)) acc in
  let sorted_int_lst = List.sort compare random_int_lst in
  List.map snd sorted_int_lst

let make_tile_stack () =
  make_tile_lst () |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

let draw_tile (stack : t Stack.t) =
  try Stack.pop stack with Stack.Empty -> raise NotEnoughTiles

(* [make_rack_aux stack acc i] is a helper to [make_tile_rack]. Draws
   [i] number of tiles from the [stack].*)
let rec make_rack_aux stack acc = function
  | 0 -> acc
  | i -> make_rack_aux stack (draw_tile stack :: acc) (i - 1)

let make_tile_rack stack =
  if Stack.length stack >= 14 then make_rack_aux stack [] 14
  else raise NotEnoughTiles

let rec numbers_of_t acc = function
  | [] -> List.rev acc
  | Tile t :: tail -> numbers_of_t (t.number :: acc) tail
  | Joker t :: tail -> numbers_of_t (t.number :: acc) tail

let rec colors_of_t acc = function
  | [] -> List.rev acc
  | Tile t :: tail -> colors_of_t (t.color :: acc) tail
  | Joker t :: tail -> colors_of_t (t.color :: acc) tail
