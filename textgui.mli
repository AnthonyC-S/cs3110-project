(** [clear_board] is a terminal window that has been reset, meaning the
    text has been cleared from the screen, the window size has been
    reset, and the cursor position is set to the top left. *)
val clear_board : unit -> unit

val build_board : State.s -> string -> string

val welcome_board : string

val g : string -> string

val ip : string

val win_board : string -> string
