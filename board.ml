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

(*Helper for [compare_tiles] let get_number x = match x with |Tile
  {number : int; color : color;} -> number |Joker {number : int; color :
  color;} -> number

  let compare_tiles x y = if (get_number x) > (get_number y) then 1 else
  -1 *)

(* compare function should sort by num first then color*)

(* Most board manipulation methods can be simplified with List.map where
   the map function checks if the row of a certain b_row matches
   row_letter and returning a different b_row if so.*)

let add_tile tile rl b =
  List.map
    (fun x ->
      if x.row = rl && List.length x.tiles == 13 then
        raise (RowAlreadyFull rl)
      else if x.row = rl then { row = x.row; tiles = tile :: x.tiles }
      else x)
    b

(* let rec add_tile tile row_letter acc = function | [] -> raise
   (InvalidBoardRow row_letter) | { row = r; tiles = ts } :: t -> if r =
   row_letter && List.length ts == 13 then raise RowAlreadyFull else if
   r = row_letter && List.length ts < 13 then acc @ ({ row = row_letter;
   tiles = tile :: ts } :: t) else add_tile tile row_letter (acc @ [ {
   row = r; tiles = ts } ]) t *)

(* [replace_tile_by_index tile row_letter acc index st.current_board]
   replaces the old tile with a new [tile] in [st.current_board] at
   [row_letter] and at the [index]. Specifically, this is used to assign
   Jokers a number or color, no other tiles should ever need to be
   replaced. *)

(* Most board manipulation methods can be simplified with List.map where
   the map function checks if the row of a certain b_row matches
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

(* Most board manipulation methods can be simplified with List.map where
   the map function checks if the row of a certain b_row matches (*
   row_letter and returning a different b_row if so.*) let rec
   remove_tile tile row_letter acc = function | [] -> raise
   InvalidBoardRow | { row = r; tiles = ts } :: t -> if r = row_letter
   then acc @ { row = row_letter; tiles = List.filter (fun x -> x <>
   tile) ts; } :: t else remove_tile tile row_letter (acc @ [ { row = r;
   tiles = ts } ]) t *)

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

let rec valid_run_aux acc = function
  | [ h ] -> acc
  | [ h; h2 ] -> h + 1 = h2 && acc
  | h :: h2 :: t -> valid_run_aux (h + 1 = h2 && acc) t
  | [] -> acc

let valid_run lst =
  let len = List.length lst in
  len >= 3
  && colors_of_t [] lst |> List.sort_uniq compare |> List.length = 1
  && valid_run_aux true (List.sort compare (numbers_of_t [] lst))

(*acc is true let rec valid_board acc = function | [] -> acc | h :: t ->
  valid_board ((List.length h = 0 || valid_run h || valid_group h) &&
  acc) t *)

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

exception EmptyBoard

let rec sort_board_by_num acc = function
  | [] -> acc
  | h :: t ->
      sort_board_by_num
        (acc @ [ { h with tiles = sort_by_number h.tiles } ])
        t
