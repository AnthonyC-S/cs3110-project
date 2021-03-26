type b

type b_row

val init_board : unit -> b

val add_tile :
  Tile.t -> string -> b_row list -> b_row list -> b_row list

val remove_tile :
  Tile.t -> string -> b_row list -> b_row list -> b_row list

val valid_group : Tile.t list -> bool

val valid_run : Tile.t list -> bool

val valid_board : bool -> Tile.t list list -> bool
