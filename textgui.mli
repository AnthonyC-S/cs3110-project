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
    the game winner, scores and asking if user would like to play a new
    game. *)
val win_board : State.s -> string -> string

(** [string_of_tile i t] is the string of spaces needed for formatting
    the terminal board, based on the needed spaces [i] and the tile's
    [t] number. *)
val string_of_tile : int -> Tile.t -> string

(* [build_board st msg] is the string representation of the game defined
   in current state [st] followed by a message [msg] either asking user
   for a command or stating error in a previous command. val build_board
   : State.s -> string -> string

   (** [welcome_board] is a string representation of the game's title
   screen. *) val welcome_board : string

   (** [g s] is the string [s] in green. *) val g : string -> string

   (** [ip] is a string indicates when user input is being asked for. *)
   val ip : string

   (** [win_board st msg] is a string representation of the game at
   state [st] where a player has emptied their rack and won. This
   consists of a message for the winner, total scores so far [msg], and
   a prompt for the player to either start a new round or end the game.
   *) val win_board : State.s -> string -> string

   (** [string_of_tile idx_count tile] is the string representation of
   [tile] with text color corresponding to the tile's color and the
   correct number of spaces concatenated to keep the spacing between
   tiles consistent on the board. *) *)
