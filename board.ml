open Tile

type board_row = {
  row : string;
  visible : bool;
  tiles : t list;
}

type board = board_row list

let rows =
  (List.init 26 (( + ) 65)
  |> List.map Char.chr
  |> List.map (String.make 1))
  @ (List.init 9 (( + ) 65)
    |> List.map Char.chr
    |> List.map (String.make 2))

let rec init_board_aux (acc : board_row list) (rows : string list) =
  match rows with
  | [] -> acc
  | str :: t ->
      init_board_aux
        ({ row = str; visible = false; tiles = [] } :: acc)
        t

let init_board () = List.rev (init_board_aux [] rows)
