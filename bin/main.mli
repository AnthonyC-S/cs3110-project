(** Starts the game and handles all gameplay actions.

    This module initalizes a new game, handles user terminal input
    commands by sending commands to [command], manages starting and
    ending turns, determining player start order, catches exceptions,
    calculates scores, and displays messages and the current game state
    through [textgui]. [main] is the highest level module in the
    program. *)

val main : unit -> unit
