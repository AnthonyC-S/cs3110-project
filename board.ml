open Tile

(** Need Board Module Description *)

type b_row = {
  row : string;
  tiles : t list;
}

type b = b_row list

exception InvalidBoardRow of string

exception RowAlreadyFull of string

let rows =
  (List.init 26 (( + ) 65)
  |> List.map Char.chr
  |> List.map (String.make 1))
  @ [ "!"; "@"; "#"; "$"; "%"; "^"; "&"; "?" ]

let rec init_board_aux (acc : b_row list) (rows : string list) =
  match rows with
  | [] -> acc
  | str :: t -> init_board_aux ({ row = str; tiles = [] } :: acc) t

let init_board () = List.rev (init_board_aux [] rows)

let add_tile tile rl b =
  List.map
    (fun x ->
      if x.row = rl && List.length x.tiles == 13 then
        raise (RowAlreadyFull rl)
      else if x.row = rl then { row = x.row; tiles = tile :: x.tiles }
      else x)
    b

(* Note, most board manipulation methods can be simplified with List.map
   where the map function checks if the row of a certain b_row matches
   row_letter and returning a different b_row if so.*)
let rec replace_tile_by_index tile row_letter acc index = function
  | [] -> raise (InvalidBoardRow row_letter)
  | { row = r; tiles = ts } :: t ->
      if r = row_letter then
        acc
        @ {
            row = row_letter;
            tiles =
              List.filteri (fun i _ -> i < index - 1) ts
              @ [ tile ]
              @ List.filteri (fun i _ -> i >= index - 1) ts;
          }
          :: t
      else add_tile tile row_letter t

let remove_tile tile rl b =
  List.map
    (fun x ->
      if x.row = rl then
        {
          row = x.row;
          tiles = List.filter (fun t -> t <> tile) x.tiles;
        }
      else x)
    b

let valid_group lst =
  let len = List.length lst in
  (len = 3 || len = 4)
  && numbers_of_t [] lst |> List.sort_uniq compare |> List.length = 1
  && colors_of_t [] lst |> List.sort_uniq compare |> List.length = len

let rec valid_run_aux = function
  | [ h ] -> true
  | h :: t -> if h + 1 <> List.hd t then false else valid_run_aux t
  | [] -> failwith "Should never be empty."

let valid_run lst =
  let len = List.length lst in
  len >= 3
  && colors_of_t [] lst |> List.sort_uniq compare |> List.length = 1
  && valid_run_aux (List.sort compare (numbers_of_t [] lst))

let rec tiles_of_board board =
  match board with [] -> [] | h :: t -> h.tiles :: tiles_of_board t

let rec valid_rows acc tile_rows =
  match tile_rows with
  | [] -> acc
  | h :: t ->
      valid_rows
        ((List.length h = 0 || valid_run h || valid_group h) && acc)
        t

let valid_board board =
  let tile_rows = tiles_of_board board in
  valid_rows true tile_rows

let rec sort_board_by_num acc = function
  | [] -> acc
  | h :: t ->
      sort_board_by_num
        (acc @ [ { h with tiles = sort_by_number h.tiles } ])
        t
