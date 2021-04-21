(** Need Tile Module Description *)

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

let s =
  List.init 14 (( + ) 1)
  |> List.map (fun i -> string_of_int i ^ if i < 10 then "  " else " ")

let make_t t n c =
  match t with
  | "T" -> Tile { number = n; color = c }
  | "J" -> Joker { number = n; color = c }
  | _ -> failwith "Not a Tile or Joker"

let joker = make_t "J" 100 None

let update_joker n c = function
  | Joker t ->
      if n < 1 || n > 13 then raise NotAJoker else make_t "J" n c
  | Tile t -> raise NotAJoker

(*[make_tile_aux acc n_lst c] is a helper to [make_tile_lst]. [acc] is
  the accumulator, [n_lst] is list 0..13, and [c] is the current color.*)
let rec make_tile_aux acc n_lst c =
  match n_lst with
  | [] -> acc
  | n :: ns -> make_tile_aux (make_t "T" n c :: acc) ns c

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
let shuffle_tile_lst (t_lst : t list) =
  Random.self_init ();
  let random_int_lst = List.map (fun x -> (Random.int 1000, x)) t_lst in
  let sorted_int_lst = List.sort compare random_int_lst in
  List.map snd sorted_int_lst

let make_tile_stack () =
  make_tile_lst () |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

let draw_tile (stack : t Stack.t) =
  try Stack.pop stack with Stack.Empty -> raise NotEnoughTiles

let tile_stack_size (stack : t Stack.t) = Stack.length stack

(* [make_rack_aux stack acc i] is a helper to [make_tile_rack]. Gives
   resulting tiles after drawing [i] number of tiles from the [stack].*)
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

(** [get_tile_color t] is the color of tile [t]. *)
let get_tile_color = function Tile t | Joker t -> t.color

(** [get_tile_number t] is the number of tile [t]. *)
let get_tile_number = function Tile t | Joker t -> t.number

(** [group_by_color c acc tlst] is list of tiles [tlst'] that contains
    tiles from [tlst] with color [c]. *)
let rec group_by_color c acc = function
  | [] -> acc
  | h :: t ->
      if get_tile_color h = c then group_by_color c (h :: acc) t
      else group_by_color c acc t

(** [tile_num_compare x y] is 0 if the number of tile [x] is the same as
    that of tile [y]. If the number of tile [x] is smaller than that of
    tile [y], this is -1. If the number of tile [x] is greater than that
    of [y], this is 1. *)
let tile_num_compare x y =
  let x_num = get_tile_number x and y_num = get_tile_number y in
  compare x_num y_num

(** [num_sort lst] is [lst] with elements of [lst] sorted by
    incrementing number order. *)
let num_sort lst = List.sort tile_num_compare lst

(** [color_hier] is a list of all possible color types for tiles. *)
let color_hier = [ Black; Red; Blue; Orange; None ]

(** [color_hier_to_num] is an associative list that represents the
    hierarchy of the colors. Black has the highest hierarchy, then
    follows Red, Blue, Orange, None.*)
let color_hier_to_num =
  [ (Black, 0); (Red, 1); (Blue, 2); (Orange, 3); (None, 4) ]

(** [sort_by_color_aux tlst colors acc] is [tlst'] with tiles sorted by
    colors accroding to the hierarchy specified in the associative color
    list [colors] and sorted by incrementing number in each color group. *)
let rec sort_by_color_aux tlst colors acc =
  match colors with
  | [] -> acc
  | h :: t ->
      tlst |> group_by_color h [] |> num_sort |> ( @ ) acc
      |> sort_by_color_aux tlst t

(** [sort_by_color tlst] is [tlst'] with the tiles inside sorted by
    colors in the order Black, Red, Blue, Orange, None respectively.
    Each group of tiles sorted by color is then sorted by incrementing
    number order with unset Joker tile coming last. *)
let sort_by_color tlst = sort_by_color_aux tlst color_hier []

(** [compare_color x y] is 0 if the color of tile [x] is the same as
    that of tile [y]. If the hierarchy of the color of tile [x] is
    smaller than that of tile [y], this is -1. If the hierarchy of the
    color of tile [x] is greater than that of [y], this is 1. *)
let compare_color x y =
  let x_color = List.assoc (get_tile_color x) color_hier_to_num
  and y_color = List.assoc (get_tile_color y) color_hier_to_num in
  compare x_color y_color

(** [same_num_sort_color n acc tlst] is [tlst'] with tiles of same
    number [n] sorted by the color hierarchy. *)
let rec same_num_sort_color n acc = function
  | [] -> List.sort compare_color acc
  | h :: t ->
      let h_num = get_tile_number h in
      if h_num = n then same_num_sort_color n (h :: acc) t
      else if h_num < n then same_num_sort_color n acc t
      else List.sort compare_color acc

(** [sort_by_number_aux tlst nlst acc] is [tlst'] with all tiles of
    [tlst] sorted in incrementing number order, and each group of the
    same number tiles are sorted by the number hierarchy. [nlst] is an
    int list that contains all the numbers of the tiles in [tlst].
    Require: [nlst] and [tlst] is sorted by incrementing number order,
    and [nlst] doesn't contain duplicates. *)
let rec sort_by_number_aux tlst nlst acc =
  match nlst with
  | [] -> acc
  | n :: t ->
      tlst
      |> same_num_sort_color n []
      |> ( @ ) acc
      |> sort_by_number_aux tlst t

(** [sort_by_number tlst] is [tlst] with the tiles inside sorted by
    incrementing number order with Joker coming last. Then, tiles of
    same number are then sorted by the color order Black, Red, Blue,
    Orange, None. *)
let sort_by_number tlst =
  let nums = List.sort_uniq compare (numbers_of_t [] tlst) in
  sort_by_number_aux (num_sort tlst) nums []

(** [tiles_of_string t] is a string representation of tile [t]. If the
    [t] is a normal tile, the number field of [t] is stringified. If [t]
    is a Joker tile, "J" is returned. *)
let tile_of_string = function
  | Tile t -> string_of_int t.number
  | Joker t -> "J"

(** [string_lst_13 slst] is a list of string [slst] with exactly 13
    string elements. If [slst] has less than 13 elements, a string with
    3 empty spaces is added to the end of [slst]. *)
let rec string_lst_13 slst =
  if List.length slst < 9 then string_lst_13 (slst @ [ "   " ])
  else if List.length slst < 13 then string_lst_13 (slst @ [ "    " ])
  else slst

(** [tiles_of_string_lst acc tlst] is a string representation of list of
    tiles [tlst]. *)
let rec tiles_of_string_lst acc = function
  | [] -> string_lst_13 (List.rev acc)
  | h :: t ->
      let new_acc = tile_of_string h :: acc in
      tiles_of_string_lst new_acc t

let get_tile_of_index index t_lst =
  match List.filteri (fun i _ -> i = index - 1) t_lst with
  | h :: t -> h
  | [] -> raise InvalidTile
