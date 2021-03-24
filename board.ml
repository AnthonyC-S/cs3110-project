open Tile

type board_row = {
  row : string;
  visible : bool;
  tiles : tile list;
}

(* Will come back later and figure out a more elegent way to generate
   this list.*)
let rows =
  [
    "A";
    "B";
    "C";
    "D";
    "E";
    "F";
    "G";
    "H";
    "I";
    "J";
    "K";
    "L";
    "M";
    "N";
    "O";
    "P";
    "Q";
    "R";
    "S";
    "T";
    "U";
    "V";
    "W";
    "X";
    "Y";
    "Z";
    "AA";
    "AB";
    "AC";
    "AD";
    "AE";
    "AF";
    "AG";
    "AH";
    "AI";
  ]

let rec init_board (acc : board_row list) (rows : string list) =
  match rows with
  | [] -> acc
  | str :: t ->
      init_board ({ row = str; visible = false; tiles = [] } :: acc) t

let make_board () = List.rev (init_board [] rows)
