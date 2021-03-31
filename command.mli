type move_phrase = {
  from_board : (string * int) list;
  from_rack : int list;
  to_row : string;
}

(* type object_phrase = string list type command = object_phrase list *)

(* type move_phrase = string list *)

type command =
  | Move of move_phrase
  | Undo
  | Reset
  | SortByNumber
  | SortByColor
  | Draw
  | EndTurn
  | Help
  | Quit

exception BlankInput

exception Malformed

exception NameTooLong

val parse : string -> command

val parse_start : string -> (int * string) list
