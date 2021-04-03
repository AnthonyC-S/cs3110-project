type move_phrase = {
  from_board : (string * int) list;
  from_rack : int list;
  to_row : string;
}

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

exception NotUniqueNames

val parse : string -> command

val parse_start : string -> (int * string) list
