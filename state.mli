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

(** Raised when a player attempts to end their turn and they have either
    not already completed a successful meld previously or the
    runs/groups played on the board do sum to 30 or more points. *)
exception InvalidMeld

(** Raised when the current player [p] has already drawn from the stack. *)
exception AlreadyDrawn of string

(** Raised when the current player [p] already made a move but tried to
    draw a tile. *)
exception AlreadyMoved

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

(** [assign_joker_in_rack st n c idx] is the new state with a joker in
    the current player's rack at index [idx] has the new tile color [c]
    and tile number [n]. Used to help determine if a runs and groups
    played to the board are valid by allowing the player to set the
    Joker's color and number. *)
val assign_joker_in_rack : s -> int -> Tile.color -> int -> s

(** [move cp s] is the state after completing a tile(s) move consisting
    of the parsed move commands in [cp] for the current game state [st].
    Moves can be either from the rack or on the board, if player has a
    valid meld. *)
val move : Command.move_phrase -> s -> s

val draw : s -> s

val get_current_player : s -> Player.p

val sort_rack_by_color : s -> s

val sort_rack_by_num : s -> s

val check_valid : s -> Player.p -> bool

val end_turn_new_st : s -> s
