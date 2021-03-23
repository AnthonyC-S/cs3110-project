(** Representation of static tile data.

    This module represents the data tile used in the game, including the
    initilizing of new tiles and complete pile of tiles (106 tiles),
    shuffling the pile, initilizing a tile stack to be used as a draw
    pile, and making a new rack from the draw pile of 14 tiles. *)

(** The abstract type of values representing tiles. *)
type tile

(** The abstract type of colors used for tiles, and None for
    representing new jokers. *)
type color

(** Raised when an stack does not have enough tiles for [draw_tile] or
    [make_tile_rack]. *)
exception NotEnoughTiles

(** [n_lst] is a list of the numbers used in Rummikub, 1..13. *)
val n_lst : int list

(** [c_lst] is a list of the four color types used in Rummikub and None
    to represent unassigned Jokers. *)
val c_lst : color list

(** [joker] is tile representing an unassigned Joker. *)
val joker : tile

(** [make_tile_lst] is the 106 ordered tiles representing a full
    Rummikub pile. *)
val make_tile_lst : unit -> tile list

(** [shuffle_tile_lst tl] is the randomly ordered tiles from [tl] the
    tile list. *)
val shuffle_tile_lst : tile list -> tile list

(** [make_tile_stack] is the full Rummikub pile randomly sorted in a
    stack, will be used as the draw pile make player racks and drawing a
    tile to end turn. *)
val make_tile_stack : unit -> tile Stack.t

(** [draw_tile ts] is a single random tile drawn from the top of tile
    stack [ts]. Raises [NotEnoughTiles] if stack is empty. *)
val draw_tile : tile Stack.t -> tile

(** [make_title_rack ts] is the 14 random tiles to form a new rack for a
    player, drawn from the top of tile stack [ts]. Raises
    [NotEnoughTiles] if the stack does not contain at least 14 tiles. *)
val make_tile_rack : tile Stack.t -> tile list
