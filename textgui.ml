open Tile
open Board

exception EmptyBoard

(** *)
let row_of_string = function
  | [] -> raise EmptyBoard
  | h :: t -> failwith "todo"
