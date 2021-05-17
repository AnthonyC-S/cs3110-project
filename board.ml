open Tile

type b_row = {
  row : string;
  tiles : t list;
}

type b = b_row list

exception InvalidBoardRow of string

exception RowAlreadyFull of string

exception EmptyBoard

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

(** [assign_jokers b_row] is the new board row with two Joker tile
    assigned to a numbers and colors that result in a valid run or
    group. If it is not possible to form a valid run or group with the
    given tiles, the unaltered board row is returned. *)
let assign_jokers b_row =
  let tile_lst_no_jokers = List.tl b_row.tiles |> List.tl in
  let rec assign_first_joker = function
    | [] -> b_row
    | joker_one :: t1 ->
        let one_joker_lst = joker_one :: tile_lst_no_jokers in
        assign_second_joker one_joker_lst t1 (make_joker_options ())
  and assign_second_joker one_joker_lst t1 = function
    | [] -> assign_first_joker t1
    | joker_two :: t2 ->
        let two_joker_lst = joker_two :: one_joker_lst in
        if valid_run two_joker_lst || valid_group two_joker_lst then
          { b_row with tiles = two_joker_lst }
        else assign_second_joker one_joker_lst t1 t2
  in
  assign_first_joker (make_joker_options ())

(** [assign_joker b_row] is the new board row with a single Joker tile
    assigned to a number and color that results in a valid run or group.
    If it is not possible to form a valid run/group with the given
    tiles, the unaltered board row is returned. *)
let assign_joker b_row =
  let tile_lst_no_joker = List.tl b_row.tiles in
  let rec find_valid_joker = function
    | [] -> b_row
    | joker :: t ->
        let r = joker :: tile_lst_no_joker in
        if valid_run r || valid_group r then { b_row with tiles = r }
        else find_valid_joker t
  in
  find_valid_joker (make_joker_options ())

(** [count_jokers b_row] is the altered board row with jokers (either
    one or or two) assigned to colors and numbers that form valid runs
    or groups. If a valid run or group is not possible, the unaltered
    board row is returned. *)
let count_jokers b_row =
  let tiles = b_row.tiles |> List.sort compare |> List.rev in
  match tiles with
  | [ Joker _ ] -> assign_joker { b_row with tiles }
  | Joker _ :: Tile _ :: tail -> assign_joker { b_row with tiles }
  | Joker _ :: Joker _ :: tail -> assign_jokers { b_row with tiles }
  | _ -> b_row

let add_tile tile rl b =
  List.map
    (fun x ->
      if x.row = rl && List.length x.tiles == 13 then
        raise (RowAlreadyFull rl)
      else if x.row = rl then
        count_jokers { x with tiles = tile :: x.tiles }
      else x)
    b

let remove_tile tile rl b =
  List.map
    (fun x ->
      if x.row = rl then
        count_jokers
          {
            row = x.row;
            tiles = List.filter (fun t -> t <> tile) x.tiles;
          }
      else x)
    b
