(** Representation of the game state.

    This module contains all information needed to start, continue or
    resume gameplay of a current game. Including information on the
    turn, board layout, player information, the tile stack and previous
    game states if needed to undo actions. *)

(** [s] is the type game state and gives the current status of game
    play. *)
type s = {
  current_turn : int;
  board : Board.b;
  players : Player.p list;
  t_stack : Tile.t Stack.t;
  past_state : s list;
}

(** [HaveNotPlayedMeld] is raised when a player attempts to move a board
    tile and has not played a successful meld. *)
exception HaveNotPlayedMeld

(** [InvalidMeld] is raised at end of turn if there is an invalid meld
    or [played_valid_meld] is false when the player moved tiles to the
    board.*)
exception InvalidMeld

(** [AlreadyDrawn s] is raised when the current player [p] has already
    drawn from the stack. *)
exception AlreadyDrawn of string

(** [AlreadyMoved] is raised when the current player [p] already made a
    move but tried to draw a tile. *)
exception AlreadyMoved

(** [init_state pl] is the initial game state for a new game of Camlcub.
    [pl] contains the number of players, either 1-2 or 1-4 and the
    player's assigned or default names. *)
val init_state : (int * string) list -> s

(** [init_new_round st] is the initial state for a new round of a just
    completed game. The names of the players in [st] and their current
    scores will be retained. All other fields will be reset and a new
    tile stack will be created. *)
val init_new_round : s -> s

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

(** [draw st] is a new state with a tile popped off the tile stack and
    added to the current player's rack of [st]. *)
val draw : s -> s

(** [get_current_player st] is the player that is playing in the current
    turn in the state [st]. *)
val get_current_player : s -> Player.p

(** [sort_rack_by_color st] is the new state with the current player's
    rack sorted by color primary and by ascending number secondary. *)
val sort_rack_by_color : s -> s

(** [sort_rack_by_num st] is the new state with the current player's
    rack sorted by ascending number as primary and by color secondary. *)
val sort_rack_by_num : s -> s

(** [check_valid p st] is true iff the tiles on the board are valid at
    the end of turn and if there is a valid meld per game rules. *)
val check_valid : Player.p -> s -> bool

(** [end_turn st] is a new state with the next in-order player as the
    now current player. The [past_state] field is reset to empty for the
    next turn. If the previous player played a meld, their player field
    [played_valid_meld] will be updated. Raises InvalidBoardSets if
    there are any invalid runs or groups on the board. Raises
    InvalidMeld if the previous player had not yet played a successful
    meld and they played tiles to the board that do not sum to at least
    30. *)
val end_turn_st : s -> s

(** [update_end_game_scores st] is new state with the player list
    updated with each player's score at the end of the game. *)
val update_end_game_scores : s -> s
