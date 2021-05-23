(** Representation of commands accepted in the game.

    This module represents the commands used in the game and manages how
    user input is parsed and connected to a meaningful value within the
    game. The module also takes care of initializing the player objects
    according to the number of players playing the game that was passed
    in as user input when first initializing the game. *)

(** The type [move_phrase] represents the sturcture of the [Move]
    command. The from_board field is (k, v) list [blst] where [k] is the
    row name and [v] is the index where the tile is located on the
    board. The from_rack field is an int list [rlst] with each element
    being the index number of the tile selected from the rack. The
    to_row field is the row letter [r] that indicates where the tiles
    referenced in from_board and from_rack are being moved to. *)
type move_phrase = {
  from_board : (string * int) list;
  from_rack : int list;
  to_row : string;
}

(** The type [command] represents all possible commands that are used in
    the the game. *)
type command =
  | Move of move_phrase
  | Undo
  | Reset
  | SortByNumber
  | SortByColor
  | Draw
  | EndTurn
  | Score
  | Help
  | Quit

(** [BlankInput] is raised when the user-entered command is an empty
    string or a sequence of whitespaces. *)
exception BlankInput

(** [Malformed] is raised when the possible commands are not used
    correctly. *)
exception Malformed

(** [NameTooLong] is raised when the user-entered player's name is more
    than or equal to 20 characters. *)
exception NameTooLong

(** [NotUniqueNames] is raised when there exist players with the same
    name. *)
exception NotUniqueNames

(** [InvalidMoveMissingTo] is raised when the [Move] command is missing
    the "to" keyword in its use. *)
exception InvalidMoveMissingTo

(** [EmptyMove] is raised when only the "move" command was entered
    without any other information following. *)
exception EmptyMove

(** [EmptyMoveFrom] is raised when no tiles are selected in the use of
    [Move] command. Ex: "move to A" *)
exception EmptyMoveFrom

(** [InvalidMoveFrom slst] is raised when there exists a reference to a
    tile index that doesn't fit the correct format in a [Move] command.
    [slst] is the list of malformed tile location references in the
    command. Ex: "move ab2 to G" *)
exception InvalidMoveFrom of string list

(** [DuplicateMoveFrom] is raised when the same tile location was
    referred to more than once in a single [Move] command. *)
exception DuplicateMoveFrom of string list

(** [InvalidMoveTo slst] is raised when the row name on the board in a
    [Move] command does not exist. [slst] is the list of malformed row
    names in the command. *)
exception InvalidMoveTo of string list

(** [MultipleMoveTo slst] is raised when there are more than 2 row
    letters in a [Move] command. A single [Move] command can only move
    tiles to one row on the board. [slst] contains all the rows the user
    entered in the single [Move] command. Ex: "move 1 3 a1 to Gi", row
    "Gi" does not exist. *)
exception MultipleMoveTo of string list

(** [EmptyMoveTo] is raised when the row letter is not specified in a
    [Move] command. Ex: "move 1 3 to " *)
exception EmptyMoveTo

(** [parse s] is a command [c]. Let [slst] be the list of words in [s].
    If [slst] is equivalent to [\["quit"\]], [Quit] is returned. If
    [slst] is equivalent to [\["move"\]], [EmptyMove] is raised. If
    [slst] is equivalent to ["move" :: t], [t] is parsed as a [Move i]
    command. if [slst] is equivalent to [\["undo"\]], [Undo] is
    returned. If [slst] is equivalent to [\["reset"\]], [Reset] is
    returned. If [slst] is equivalent to [\["colorsort"\]],
    [SortByColor] is returned. If [slst] is equivalent to
    [\["numbersort"\]], [SortByNumber] is returned. If [slst] is
    equivalent to [\["draw"\]], [Draw] is returned. If [slst] is
    equivalent to [\["endturn"\]], [EndTurn] is returned. If [slst] is
    equivalent to [\["help"\]], [Help] is returned. In all other cases,
    [Malformed] exception is raised. *)
val parse : string -> command

(** [parse_start s] is string list [slst] . If [s] contains information
    for the players, [slst] is a ([k], [v]) pair list where [k] is the
    player number and [v] is the player name. The length of [slst]
    differs by the number of players. If the first word in [s] is
    equivalent to "quit", game terminates immediately. *)
val parse_start : string -> (int * string) list
