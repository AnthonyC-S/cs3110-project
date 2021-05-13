type color =
  | Blue
  | Orange
  | Red
  | Black
  | None

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

exception InvalidIndex of (string * int)

(** [n_lst] is a list of the numbers used in Rummikub, 1..13. *)
let n_lst = List.init 13 (( + ) 1)

(** [c_lst] is a list of the four [color] types used for Tile type
    tiles. *)
let c_lst = [ Blue; Orange; Red; Black ]

let make_t t n c =
  match t with
  | "T" -> Tile { number = n; color = c }
  | "J" -> Joker { number = n; color = c }
  | _ -> raise InvalidTile

(** [joker] is a Joker tile [t] with number [100] and color [None] for
    initializing an unset Joker tile. *)
let joker = make_t "J" 100 None

let update_joker n c = function
  | Joker t ->
      if n < 1 || n > 13 then raise NotAJoker else make_t "J" n c
  | Tile t -> raise NotAJoker

(** [make_tile_aux acc n_lst c] is a Tile type tile list [tlst] with all
    numbers in integer list [n_lst] with color [c]. This is a helper to
    [make_tile_lst]. *)
let rec make_tile_aux acc n_lst c =
  match n_lst with
  | [] -> acc
  | n :: ns -> make_tile_aux (make_t "T" n c :: acc) ns c

let make_joker_options () =
  let rec make_jokers acc n_lst c =
    match n_lst with
    | [] -> acc
    | n :: ns -> make_jokers (make_t "J" n c :: acc) ns c
  in
  List.concat_map (make_jokers [] n_lst) c_lst

(** [make_tile_lst ()] is the 106 ordered tile list [t_lst] representing
    a full Rummikub pile. Ordered as with two unset jokers at the head
    and then sets of tiles with numbers 1..13 in the following color
    order: Blue, Orange, Red, Black, Blue, Orange, Red, Black. *)
let make_tile_lst () =
  joker :: joker
  :: List.concat_map (make_tile_aux [] n_lst) (c_lst @ c_lst)

(** [shuffle_tile_lst t_lst] is the randomly ordered tiles [t_lst'] from
    the tile list [t_lst]. Note: this function was modified from here:
    https://stackoverflow.com/a/15095713. *)
let shuffle_tile_lst (t_lst : t list) =
  Random.self_init ();
  let random_int_lst = List.map (fun x -> (Random.int 1000, x)) t_lst in
  let sorted_int_lst = List.sort compare random_int_lst in
  List.map snd sorted_int_lst

let make_tile_stack () =
  make_tile_lst () |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

let make_ordered_tile_stack () =
  make_tile_lst () |> List.to_seq |> Stack.of_seq

let p_order_tile_stack () =
  make_tile_aux [] [ 1; 5; 9 ] Blue
  @ make_tile_aux [] [ 2; 6 ] Orange
  @ make_tile_aux [] [ 3; 7 ] Red
  @ make_tile_aux [] [ 4; 8 ] Black
  |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

let draw_tile (stack : t Stack.t) =
  try Stack.pop stack with Stack.Empty -> raise NotEnoughTiles

let tile_stack_size (stack : t Stack.t) = Stack.length stack

(* [make_rack_aux s acc n] is a tile list [t_lst] with [n] number of
   tiles drawn from the stack of tiles [s]. This is a helper to
   [make_tile_rack]. *)
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

(** [num_sort t_lst] is tile list [t_lst'] with tiles of [t_lst] sorted
    by their numbers in incrementing order. *)
let num_sort lst = List.sort tile_num_compare lst

(** [color_hier_to_num] is an associative list of color-integer pairs
    [c_hier_lst] that represents the hierarchy of the colors. Black has
    the highest hierarchy with its value being 0, then follows Red,
    Blue, Orange, None.*)
let color_hier_to_num =
  [ (Black, 0); (Red, 1); (Blue, 2); (Orange, 3); (None, 4) ]

(** [color_hier] is a color list [c_lst] of all possible color types for
    tiles including None type. *)
let color_hier = List.map (fun (k, _) -> k) color_hier_to_num

(** [sort_by_color_aux tlst colors acc] is tile list [tlst'] with tiles
    sorted by colors accroding to the hierarchy specified in the
    associative list of color-integer pairs [colors] and then sorted by
    incrementing number in each color group. This is a helper to
    [sort_by_color]. *)
let rec sort_by_color_aux tlst colors acc =
  match colors with
  | [] -> acc
  | h :: t ->
      tlst |> group_by_color h [] |> num_sort |> ( @ ) acc
      |> sort_by_color_aux tlst t

let sort_by_color tlst = sort_by_color_aux tlst color_hier []

(** [color_compare x y] is 0 if the color of tile [x] is the same as
    that of tile [y]. If the hierarchy of the color of tile [x] is
    smaller than that of tile [y], this is -1. If the hierarchy of the
    color of tile [x] is greater than that of [y], this is 1. The
    hierarchy is according to how it is specified in
    [color_hier_to num]. *)
let color_compare x y =
  let x_color = List.assoc (get_tile_color x) color_hier_to_num
  and y_color = List.assoc (get_tile_color y) color_hier_to_num in
  compare x_color y_color

(** [same_num_sort_color n acc tlst] is tile list [tlst'] with tiles of
    same number [n] in the tile list [tlst] sorted by the color
    hierarchy specified in [color_hier_to num]. *)
let rec same_num_sort_color n acc = function
  | [] -> List.sort color_compare acc
  | h :: t ->
      let h_num = get_tile_number h in
      if h_num = n then same_num_sort_color n (h :: acc) t
      else if h_num < n then same_num_sort_color n acc t
      else List.sort color_compare acc

(** [sort_by_number_aux tlst nlst acc] is tile list [tlst'] with all
    tiles of [tlst] sorted in incrementing number order and each group
    of the same number tiles sorted by the color hierarchy. [nlst] is an
    integer list with numbers in incrementing order that contains all
    the numbers of the tiles in [tlst]. Require: [nlst] and [tlst] is
    sorted by incrementing number order, and [nlst] doesn't contain
    duplicates. *)
let rec sort_by_number_aux tlst nlst acc =
  match nlst with
  | [] -> acc
  | n :: t ->
      tlst
      |> same_num_sort_color n []
      |> ( @ ) acc
      |> sort_by_number_aux tlst t

let sort_by_number tlst =
  let nums = List.sort_uniq compare (numbers_of_t [] tlst) in
  sort_by_number_aux (num_sort tlst) nums []

let get_tile_of_index (row : string) index t_lst =
  match List.filteri (fun i _ -> i = index - 1) t_lst with
  | h :: t -> h
  | [] -> raise (InvalidIndex (row, index))

let get_tile_rec = function Tile t -> t | Joker t -> t

let color_sort t_lst =
  let rec sort k r b o j = function
    | [] ->
        List.flatten
          (List.map (fun x -> List.sort compare x) [ k; r; b; o; j ])
    | h :: t -> (
        match (get_tile_rec h).color with
        | Black -> sort (h :: k) r b o j t
        | Red -> sort k (h :: r) b o j t
        | Blue -> sort k r (h :: b) o j t
        | Orange -> sort k r b (h :: o) j t
        | None -> sort k r b o (h :: j) t)
  in
  sort [] [] [] [] [] t_lst
