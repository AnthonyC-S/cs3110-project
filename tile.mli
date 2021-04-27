(** Representation of static tile data.

    This module represents the data tile used in the game, including the
    initilizing of new tiles and complete pile of tiles (106 tiles),
    shuffling the pile, initilizing a tile stack to be used as a draw
    pile, and making a new rack from the draw pile of 14 tiles. *)

(** The abstract type of values representing tiles. *)

(* type t *)

(** The abstract type of colors used for tiles, and None for
    representing unassigned jokers. *)
type color =
  | Blue
  | Orange
  | Red
  | Black
  | None

(*new*)
type t_rec = {
  number : int;
  color : color;
}

type t =
  | Tile of t_rec
  | Joker of t_rec

(** Raised when an stack does not have enough tiles for [draw_tile] or
    [make_tile_rack]. *)
exception NotEnoughTiles

(** Raised when an expected Joker tile is not a Joker. *)
exception NotAJoker

(** Raised when a tile is not a valid tile. *)
exception InvalidTile

exception InvalidIndex of (string * int)

(** [make_t n c t] is a tile with [n] for the tile number, [c] for the
    tile color, and [t] or either "T" for Tile or "J" for Joker type.
    Requires: [t] is either "T" or "J" [n] is 0 .. 13 [c] is a valid
    color type. Raises: [InvalidTile] if [t] is not a valid tile type.*)
val make_t : string -> int -> color -> t

(** [update_joker n c t] is tile representing a newly assigned Joker,
    with [n] for the tile number, [c] for the tile color, and [t] for
    the tile type. Raises [NotAJoker] if [t] is not type Joker*)
val update_joker : int -> color -> t -> t

(** [make_tile_stack] is the full Rummikub pile randomly sorted in a
    stack, will be used as the draw pile make player racks and drawing a
    tile to end turn. *)
val make_tile_stack : unit -> t Stack.t

val tile_stack_size : t Stack.t -> int

(** [draw_tile ts] is a single random tile drawn from the top of tile
    stack [ts]. Raises [NotEnoughTiles] if stack is empty. *)
val draw_tile : t Stack.t -> t

(** [make_title_rack ts] is the 14 random tiles to form a new rack for a
    player, drawn from the top of tile stack [ts]. Raises
    [NotEnoughTiles] if the stack does not contain at least 14 tiles. *)
val make_tile_rack : t Stack.t -> t list

(** [numbers_of_t a tl] is the *)
val numbers_of_t : int list -> t list -> int list

val colors_of_t : color list -> t list -> color list

val sort_by_color : t list -> t list

val sort_by_number : t list -> t list

val get_tile_of_index : string -> int -> 'a list -> 'a

val get_tile_color : t -> color

val get_tile_number : t -> int

(** [make_joker_options] is the 52 Joker list of the numbers 1..13 in
    Blue, Orange, Red, or Black. Used to assign jokers to make valid
    board rows. *)
val make_joker_options : unit -> t list

(** [get_tile_rec t] is the extraction of tile record of the tile [t],
    ie the number of color of either a Tile or Joker. *)
val get_tile_rec : t -> t_rec
