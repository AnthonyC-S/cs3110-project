(** Representation of type and functions of board used in the game.

    This module includes the initilizing of a new board, adding and
    removing of tiles from the board, and board validation and sorting. *)

(** [b_row] represents a row on the board with row letter [row] and
    [tiles], a list of tiles in that row. *)
type b_row = {
  row : string;
  tiles : Tile.t list;
}

(** [b] represents the board as a list of [b_row]. *)
type b = b_row list

(** [InvalidBoardSets slst] represents the invalid tile runs or groups
    on the board. *)
exception InvalidBoardSets of string list

(** [RowAlreadyFull] is raised when a player attempts to add a tile to a
    row that is full with 13 tiles. *)
exception RowAlreadyFull of string

(** [init_board] is the initial state of the board with no tiles. *)
val init_board : unit -> b

(** [add_tile tile rl b] is a board [b'] that is board [b] with [tile]
    added to row [rl]. Joker tiles on the board are assigned a number
    and color that form a valid or run with the rest of the tiles in its
    row. If no such values exists, joker tiles are left unaltered.
    Raises [RowAlreadyFull] if row [rl] is already full. *)
val add_tile : Tile.t -> string -> b -> b

(** [remove_tile tile rl b] is a board [b'] that is board [b] with
    [tile] removed from row [rl]. Joker tiles on the board are assigned
    a number and color that form a valid or run with the rest of the
    tiles in its row. If no such values exists, joker tiles are left
    unaltered. *)
val remove_tile : Tile.t -> string -> b -> b

(** [valid_board board] is true if each row in [board] is either empty,
    or consists of tiles making a valid run or a valid group. Raises
    [InvalidBoardRow string list] to warn user which rows are invalid on
    the board. *)
val valid_board : b -> bool

(** [sort_board_by_num acc b] is a new board [b'] consisting of the same
    rows in [b] but with the tiles sorted in incrementing number order.
    Tiles of the same number are also sorted to fit the color hierarchy. *)
val sort_board_by_num : b -> b -> b
