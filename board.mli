type b_row = {
  row : string;
  tiles : Tile.t list;
}

type b = b_row list

exception InvalidBoardRow

exception RowAlreadyFull

(** [init_board] is the initial state of the board with no tiles. *)
val init_board : unit -> b

val add_tile : Tile.t -> string -> b -> b -> b

val replace_tile_by_index : Tile.t -> string -> b -> int -> b -> b

val remove_tile : Tile.t -> string -> b -> b -> b

(** [valid_board acc board] is the result of removing a tile from the
    board *)
val valid_board : b -> bool

val sort_board_by_num : b -> b -> b
