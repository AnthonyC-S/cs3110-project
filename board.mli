(** The abstract type representing board. *)
type b

(** The type of a row on the board. *)
type board_row

(** [init_board] is the initial state of the board with no tiles. *)
val init_board : unit -> b

(* val valid_board : bool -> Tile.t list list -> bool *)

(** [valid_board acc board] is the result of removing a tile from the board *)
val valid_board : bool -> b -> bool

(** [add_tile tile row_letter acc] is the result of adding a tile to the board *)
val add_tile : Tile.t  -> string -> board_row list -> b -> b


(** [remove_tile tile row_letter acc] is the result of removing a tile from the board *)
val remove_tile : Tile.t  -> string -> board_row list -> b -> b

(* val add_tile_to_board : Tile.t -> string -> board_row list -> b -> b *)
