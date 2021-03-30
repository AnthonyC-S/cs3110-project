(* type object_phrase = string list type command = object_phrase list *)

type move_phrase = string list

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

val parse : string -> command

val parse_start : string -> (int * string) list
