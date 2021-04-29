(** Representation of static tile data.

    This module represents the data tile used in the game, including the
    initilizing of new tiles and complete pile of tiles (106 tiles),
    shuffling the pile, initilizing a tile stack to be used as a draw
    pile, and making a new rack from the draw pile of 14 tiles. *)

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

(** [make_t t_str n c] is a Tile type tile [t] with number [n] and color
    [c] if [t_str] is ["T"]. If [t_str] is ["J"], this is a Joker tile
    [t] with number [n] and color [c]. If some other string is passed,
    the raises a [InvalidTile]. *)
val make_t : string -> int -> color -> t

(** [update_joker n c t] is a Joker tile [t'] with the number updated to
    [n] and color updated to [c]. If [n] is not a valid integer value of
    the game (more than 13, less than 1), then [NotAJoker] exception is
    thrown. If [t] is not a Joker type tile, [NotAJoker] exception is
    thrown. *)
val update_joker : int -> color -> t -> t

(** [make_tile_stack ()] is a Stack [s] of randomly shuffled full pile
    of all 106 tiles. *)
val make_tile_stack : unit -> t Stack.t

(** [tile_stakc_size s] is the number of tiles in the stack [s]. *)
val tile_stack_size : t Stack.t -> int

(** [draw_tile s] is a tile [t] popped from stack of tiles [s]. If there
    aren't any tiles left in [s], [NotEnoughTiles] exception is thrown. *)
val draw_tile : t Stack.t -> t

(** [make_tile_rack s] is a tile list [t_lst] with 14 tiles drawn from
    stavk [s]. If [s] does not have enough tiles to make a 14 tile list,
    [NotEnoughTiles] exception is thrown. *)
val make_tile_rack : t Stack.t -> t list

(** [numbers_of_t acc t_lst] is an integer list [int_lst] such that each
    integer is the number of the tile in [t_lst]. The numbers are
    ordered in the same order as the tiles in [t_lst]. *)
val numbers_of_t : int list -> t list -> int list

(** [colors_of_t acc t_lst] is color list [c_lst] such that each color
    type is that of the tile in [t_lst]. The color types are ordered in
    the same order as the tiles in [t_lst]. *)
val colors_of_t : color list -> t list -> color list

(** [get_tile_number t] is the number of tile [t]. *)
val get_tile_number : t -> int

(** [sort_by_color tlst] is tile list [tlst'] with the tiles in [tlst]
    sorted by colors in the order Black, Red, Blue, Orange, None
    respectively. Each group of color-sorted tiles is then sorted by
    incrementing number order with unset Joker tile coming last. *)
val sort_by_color : t list -> t list

(** [sort_by_number tlst] is tile list [tlst'] with the tiles inside
    sorted by incrementing number order with Joker coming last. Then,
    tiles of same number are sorted by the color order Black, Red, Blue,
    Orange, None. *)
val sort_by_number : t list -> t list

(** [get_tile_of_index i t_lst] is element [e] of [t_lst] where the
    index of [e] matches [i - 1] because [i] is 1-based index value. If
    there are no elements in [t_lst] that matches the index value [i],
    [InvalidIndex] exception is raised. *)
val get_tile_of_index : string -> int -> 'a list -> 'a

(** [make_joker_options] is the 52 Joker list of the numbers 1..13 in
    Blue, Orange, Red, or Black. Used to assign jokers to make valid
    board rows. *)
val make_joker_options : unit -> t list

(** [get_tile_rec t] is the extraction of tile record of the tile [t],
    ie the number of color of either a Tile or Joker. *)
val get_tile_rec : t -> t_rec

(** [p_order_tile_stack] is a stack of tiles with single digit numbers
    that will be used for setting the random start order. *)
val p_order_tile_stack : unit -> t Stack.t
