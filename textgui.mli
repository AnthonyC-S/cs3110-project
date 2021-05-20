(** Representation of the terminal gui interface.

    This module displays the formatted game board, current player's tile
    rack, and relevelnt messages and warnings to the user. *)

(** [clear_board] is a terminal window that has been reset, meaning the
    text has been cleared from the screen, the window size has been
    reset, and the cursor position is set to the top left. *)
val clear_board : unit -> unit

(** [build_board st s] is the terminal output of the current game state
    [st] and a message to the user [s]. *)
val build_board : State.s -> string -> string

(** [welcome_board s] is the terminal output of the welcome screen to
    the user. *)
val welcome_board : string

(** [g s] is the Xterm 256 color green of the string [s]. *)
val g : string -> string

(** [ip] is the user command prompt in the game is frequently used
    before a read_line. *)
val ip : string

(** [win_board st s] is the end of game display showing the game winner,
    scores and asking if user would like to play a new game. *)
val win_board : State.s -> string -> string

(** [string_of_tile i t] is the string of spaces needed for formatting
    the terminal board, based on the needed spaces [i] and the tile's
    [t] number. *)
val string_of_tile : int -> Tile.t -> string
