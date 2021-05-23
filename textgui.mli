(** Representation of the terminal gui interface.

    This module displays the formatted game board, current player's tile
    rack, and relevelnt messages and warnings to the user. *)

(** [clear_board] is a terminal window that has been reset, meaning the
    text has been cleared from the screen, the window size has been
    reset, and the cursor position is set to the top left. *)
val clear_board : unit -> unit

(** [build_board st s] is the string representation [s'] of the current
    game state [st] and a message to the user [s] that is displayed
    below the string representation of the board in [st]. *)
val build_board : State.s -> string -> string

(** [welcome_board s] is the terminal output of the welcome screen to
    the user. *)
val welcome_board : string

(** [kw s] is the black text on white background representaion of [s]. *)
val kw : string -> string

(** [rw s] is the red text on white background representaion of [s]. *)
val rw : string -> string

(** [ow s] is the ornage text on white background representaion of [s]. *)
val ow : string -> string

(** [bw s] is the blue text on white background representaion of [s]. *)
val bw : string -> string

(** [g s] is the Xterm 256 color green of the string [s]. *)
val g : string -> string

(** [h s] is [s] formatted to be a header. *)
val h : string -> string

(** [c s] is [s] formatted to display a command to enter. *)
val c : string -> string

(** [i s] is a string italicized. *)
val i : string -> string

(** [ip] is the user command prompt in the game is frequently used
    before a read_line. *)
val ip : string

(** [win_board st s] is the end of game display string [s] that shows
    the game winner and scores for state [st] and gives the option to
    play a new game. *)
val win_board : State.s -> string -> string

(** [string_of_tile i t] is the string of spaces needed for formatting
    the terminal board, based on the needed spaces [i] and the tile's
    [t] number. *)
val string_of_tile : int -> Tile.t -> string
