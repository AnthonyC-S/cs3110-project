type object_phrase = {tiles: string list; row: string}
(* type object_phrase = string list type command = object_phrase list *)

type command = 
  | Draw
  | Move of object_phrase
  | End
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

val parse : string -> command