type object_phrase = {
  tiles : string list;
  row : string;
}

(* type object_phrase = string list type command = object_phrase list *)

(* type move_phrase = string list *)

type command =
  | Move of object_phrase
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
