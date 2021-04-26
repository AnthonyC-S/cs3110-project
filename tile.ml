(** Need Tile Module Description *)

(** [color] type represents the types of color of the tiles. The only
    valid colors are [Blue], [Orange], [Red], and [Black]. [None] is for
    first initializing the Joker tiles since the color of a Joker tile
    is set when the user uses the tile. *)
type color =
  | Blue
  | Orange
  | Red
  | Black
  | None

(** [t_rec] is all the information a tile should hold. This includes the
    [number] integer the tile holds and the [color] of the tile. *)
type t_rec = {
  number : int;
  color : color;
}

(** [t] represents tiles which has two types normal [Tile] and [Joker]. *)
type t =
  | Tile of t_rec
  | Joker of t_rec

(** The exception [NotEnoughTiles] is thrown when there aren't enough
    tile to pick from. *)
exception NotEnoughTiles

(** The exception [NotAJoker] is thrown when the tile is not a valid
    [Joker] type tile. *)
exception NotAJoker

(** The exception [InvalidTile] is thrown when the tile being searched
    doesn't not exist. *)
exception InvalidTile

(** The exception [InvalidIndex (k, v)] is thrown when the there doesn't
    exist an element on index [i] in the row named [k]. *)
exception InvalidIndex of (string * int)

(** [n_lst] is a list of the numbers used in Rummikub, 1..13. *)
let n_lst = List.init 13 (( + ) 1)

(** [c_lst] is a list of the four [color] types used for Tile type
    tiles. *)
let c_lst = [ Blue; Orange; Red; Black ]

(** [make_t t_str n c] is a Tile type tile [t] with number [n] and color
    [c] if [t_str] is ["T"]. If [t_str] is ["J"], this is a Joker tile
    [t] with number [n] and color [c]. If some other string is passed,
    the raises a [Failure]. *)
let make_t t n c =
  match t with
  | "T" -> Tile { number = n; color = c }
  | "J" -> Joker { number = n; color = c }
  | _ -> failwith "Not a Tile or Joker"

(** [joker] is a Joker tile [t] with number [100] and color [None] for
    initializing an unset Joker tile. *)
let joker = make_t "J" 100 None

(** [update_joker n c t] is a Joker tile [t'] with the number updated to
    [n] and color updated to [c]. If [n] is not a valid integer value of
    the game (more than 13, less than 1), then [NotAJoker] exception is
    thrown. If [t] is not a Joker type tile, [NotAJoker] exception is
    thrown. *)
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

(** [make_tile_stack ()] is a Stack [s] of randomly shuffled entire tile
    list of 106 tiles. *)
let make_tile_stack () =
  make_tile_lst () |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

(** [draw_tile s] is a tile [t] popped from stack of tiles [s]. If there
    aren't any tiles left in [s], [NotEnoughTiles] exception is thrown. *)
let draw_tile (stack : t Stack.t) =
  try Stack.pop stack with Stack.Empty -> raise NotEnoughTiles

(** [tile_stakc_size s] is the number of tiles in the stack [s]. *)
let tile_stack_size (stack : t Stack.t) = Stack.length stack

(* [make_rack_aux s acc n] is a tile list [t_lst] with [n] number of
   tiles drawn from the stack of tiles [s]. This is a helper to
   [make_tile_rack]. *)
let rec make_rack_aux stack acc = function
  | 0 -> acc
  | i -> make_rack_aux stack (draw_tile stack :: acc) (i - 1)

(** [make_tile_rack s] is a tile list [t_lst] with 14 tiles drawn from
    stavk [s]. If [s] does not have enough tiles to make a 14 tile list,
    [NotEnoughTiles] exception is thrown. *)
let make_tile_rack stack =
  if Stack.length stack >= 14 then make_rack_aux stack [] 14
  else raise NotEnoughTiles

(** [numbers_of_t acc t_lst] is an integer list [int_lst] such that each
    integer is the number of the tile in [t_lst]. The numbers are
    ordered in the same order as the tiles in [t_lst]. *)
let rec numbers_of_t acc = function
  | [] -> List.rev acc
  | Tile t :: tail -> numbers_of_t (t.number :: acc) tail
  | Joker t :: tail -> numbers_of_t (t.number :: acc) tail

(** [colors_of_t acc t_lst] is color list [c_lst] such that each color
    type is that of the tile in [t_lst]. The color types are ordered in
    the same order as the tiles in [t_lst]. *)
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

(** [sort_by_color tlst] is tile list [tlst'] with the tiles in [tlst]
    sorted by colors in the order Black, Red, Blue, Orange, None
    respectively. Each group of color-sorted tiles is then sorted by
    incrementing number order with unset Joker tile coming last. *)
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

(** [sort_by_number tlst] is tile list [tlst'] with the tiles inside
    sorted by incrementing number order with Joker coming last. Then,
    tiles of same number are sorted by the color order Black, Red, Blue,
    Orange, None. *)
let sort_by_number tlst =
  let nums = List.sort_uniq compare (numbers_of_t [] tlst) in
  sort_by_number_aux (num_sort tlst) nums []

(** [tiles_of_string t] is a string representation of tile [t]. If [t]
    is a Tile type tile, the number field of [t] is stringified. If [t]
    is a Joker tile, "J" is returned. *)
let tile_of_string = function
  | Tile t -> string_of_int t.number
  | Joker t -> "J"

(** [string_lst_13 slst] is a list of string [slst'] with exactly 13
    string elements. If [slst] has less than 9 elements, a string with 3
    empty spaces is added to the end of [slst]. If [slst] has more than
    or equal to 9 elements but less than 13 elements, a string with 4
    spaces is added to the end of [slst]. *)
let rec string_lst_13 slst =
  if List.length slst < 9 then string_lst_13 (slst @ [ "   " ])
  else if List.length slst < 13 then string_lst_13 (slst @ [ "    " ])
  else slst

(** [tiles_of_string_lst acc tlst] is a string representation of tile
    list [tlst] [slst]. Whitespaces strings are appended at the end of
    [slst] to always make the length of [slst] to be 13. *)
let rec tiles_of_string_lst acc = function
  | [] -> string_lst_13 (List.rev acc)
  | h :: t ->
      let new_acc = tile_of_string h :: acc in
      tiles_of_string_lst new_acc t

(** [get_tile_of_index i t_lst] is element [e] of [t_lst] where the
    index of [e] matches [i - 1] because [i] is 1-based index value. If
    there are no elements in [t_lst] that matches the index value [i],
    [InvalidIndex] exception is raised. *)
let get_tile_of_index (row : string) index t_lst =
  match List.filteri (fun i _ -> i = index - 1) t_lst with
  | h :: t -> h
  | [] -> raise (InvalidIndex (row, index))
