(** Representation of the game state.

    This module manages the state object that contains all information
    needed to start, continue or resume gameplay of a current game.
    Including information on the turn, board layout, player information,
    the tile stack and previous game states if needed to undo actions. *)

(** The type [s] represents the game state and gives the current status
    of game play. *)
type s = {
  current_turn : int;
  board : Board.b;
  players : Player.p list;
  t_stack : Tile.t Stack.t;
  past_state : s list;
}

(** [HaveNotPlayedMeld s] is raised when a player attempts to move a board
    or Joker tile and has not played a successful meld. *)
exception HaveNotPlayedMeld of string

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

(** [InvalidMeldUnemptyRow s] is raised when the current player [p] who
    hasn't met the initial meld tries to play a tile to an unempty row
    on the board. [s] is the row letter that the player try to play a
    tile to. *)
exception InvalidMeldUnemptyRow of string

(** [init_state pl] is the initial game state [st'] for a new game of
    Camlcub. [pl] contains a list of pairs [(i, name)] where [i] is the
    player number and [name] is the player name. *)
val init_state : (int * string) list -> s

(** [init_new_round st] is the initial state [st'] for a new round of a
    just completed game. The names of the players in [st] and their
    current scores will be retained. All other fields will be reset and
    a new tile stack will be created. *)
val init_new_round : s -> s

(** [undo_move st] is game state [st'] prior to the most recent
    successful [move] command. Otherwise, it is the current state [st]
    if there are no previous states to go back to. *)
val undo_move : s -> s

(** [reset_turn st] is the initial game state [st'] at the start of the
    turn for the current player [cp]. The [past_state] field in [st] is
    reset to empty, which also resets the board and [cp]'s rack to the
    initial state at the start of the [cp]'s turn. *)
val reset_turn : s -> s

(** [move cp s] is a new state [st'] after completing a tile(s) move
    according to the parsed move commands [cp] in [st]. Moves can be
    either from the rack or on the board, if the current player has a
    valid meld. If the meld is not met for the current player, they
    could only move tiles from their rack. If the current player has
    already drawn and tried to make a move, [AlreadyDrawn].
    [HaveNotPlayedMeld] is raised if the player tries to move tiles on
    the board without fulfilling the initial meld.
    [InvalidMeldUnemptyRow s] is raised when a player who hasn't met the
    initial meld yet tried to move a tile to a row where a different
    player has already played a tile to. *)
val move : Command.move_phrase -> s -> s

(** [draw st] is a new state [st'] with a tile popped from the tile
    stack and added to the current player's rack. *)
val draw : s -> s

(** [get_current_player st] is the player [p] that is currently playing
    in the state [st]. *)
val get_current_player : s -> Player.p

(** [sort_rack_by_color st] is the new state [st'] with the current
    player's rack sorted by color primary and by ascending number
    secondary. *)
val sort_rack_by_color : s -> s

(** [sort_rack_by_num st] is the new state [st'] with the current
    player's rack sorted by ascending number as primary and by color
    secondary. *)
val sort_rack_by_num : s -> s

(** [check_valid p st] is [true] iff the tiles on the board are valid at
    the end of turn and if there is a valid meld per game rules.
    Otherwise, [false] is returned. *)
val check_valid : Player.p -> s -> bool

(** [end_turn st] is a new state [st'] with the next-in-order player
    [p2] as the now current player. The past_state field of [p2] is
    reset to empty for the next turn. [st] is updated as noted
    previously when a) the stack is empty and the player has no more
    tiles to draw from or b) when the current player has drawn or has
    made a move and the tiles on the board are in a valid group or run.
    If the previous player [p1] played a meld, [p1]'s
    [played_valid_meld] field will be updated. [InvalidBoardSets] is
    raised if there are any invalid runs or groups on the board.
    [InvalidMeld] is raised if the previous player [p1] had not yet
    played a successful meld and they played tiles to the board that do
    not sum to at least 30. *)
val end_turn_st : s -> s

(** [update_end_game_scores st] is new state [st'] with the player list
    updated with each player's score. *)
val update_end_game_scores : s -> s
