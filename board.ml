open Tile

type b = board_row list

and board_row = {
  row : string;
  visible : bool;
  tiles : t list;
}

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

let rec add_tile tile row_letter acc = function
  | [] -> failwith "Row not on board."
  | { row = r; visible = v; tiles = ts } :: t ->
      if r = row_letter then
        acc
        @ {
            row = row_letter;
            visible = true;
            tiles = List.sort compare (tile :: ts);
          }
          :: t
      else
        add_tile tile row_letter
          (acc @ [ { row = r; visible = v; tiles = ts } ])
          t

let rec remove_tile tile row_letter acc = function
  | [] -> failwith "Row not on board."
  | { row = r; visible = v; tiles = ts } :: t ->
      if r = row_letter then
        acc
        @ {
            row = row_letter;
            visible = true;
            tiles = List.filter (fun x -> x <> tile) ts;
          }
          :: t
      else
        add_tile tile row_letter
          (acc @ [ { row = r; visible = v; tiles = ts } ])
          t
