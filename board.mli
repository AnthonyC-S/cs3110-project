type b_row = {
  row : string;
  visible : bool;
  tiles : Tile.t list;
}

type b = b_row list

(** [init_board] is the initial state of the board with no tiles. *)
val init_board : unit -> b

val add_tile : Tile.t -> string -> b -> b -> b

val add_tile_by_index : Tile.t -> string -> b -> int -> b -> b

val remove_tile : Tile.t -> string -> b -> b -> b

val valid_group : Tile.t list -> bool

(** [valid_board acc board] is the result of removing a tile from the
    board *)
val valid_board : b -> bool

val sort_board_by_num : b -> b -> b
