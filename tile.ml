type color =
  | Blue
  | Orange
  | Red
  | Black
  | None

type tile = {
  number : int;
  color : color;
}

type joker = {
  mutable number : int;
  mutable color : color;
}

type t =
  | Tile of tile
  | Joker of joker

let rec make_pile lst = function 0 -> lst | _ -> lst
