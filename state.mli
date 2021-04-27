(** [s] is the type game state and gives the current status of game
    play. *)
type s = {
  current_turn : int;
  board : Board.b;
  players : Player.p list;
  t_stack : Tile.t Stack.t;
  past_state : s list;
}

(** Raised when a player attempts to move a board tile and has not
    played a successful meld. *)
exception HaveNotPlayedMeld

(** Raised when a player attempts to end their turn and there are
    invalid runs or groups on the board. *)
exception InvalidBoardSets

(** Raied when a player attempts to end their turn and they have either
    not already completed a successful meld previously or the
    runs/groups played on the board do sum to 30 or more points. *)
exception InvalidMeld

(** [init_state pl] is the initial game state for a new game of Camlcub.
    [pl] contains the number of players, either 1-2 or 1-4 and the
    player's assigned or default names. *)
val init_state : (int * string) list -> s

(** [undo_move st] is game state prior to the most recent successful
    [move] command. Otherwise, it is the current state ff there are no
    previous states to go back to. *)
val undo_move : s -> s

(** [reset_turn st] is the initial game state at the start of the turn
    for the current player in [st]. Sets the record field [past_state]
    to empty, the board to the initial state at the start of the turn,
    and the current players rack to the tiles at the start of the turn. *)
val reset_turn : s -> s

(** [move cp s] is the state after completing a tile(s) move consisting
    of the parsed move commands in [cp] for the current game state [st].
    Moves can be either from the rack or on the board, if player has a
    valid meld. *)
val move : Command.move_phrase -> s -> s

val draw : s -> s

(** [get_current_player st] is the player that is playing in the current
    turn in the state [st]. *)
val get_current_player : s -> Player.p

val sort_rack_by_color : s -> s

val sort_rack_by_num : s -> s

val end_turn : s -> s
