type b

type board_row

val init_board : unit -> b

val valid_group : Tile.t list -> bool

val valid_run : Tile.t list -> bool

val valid_board : bool -> Tile.t list list -> bool

(* val add_tile_to_board : Tile.t -> string -> board_row list -> b -> b *)
