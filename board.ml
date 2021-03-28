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

(*Helper for [compare_tiles]
let get_number x = 
  match x with 
  |Tile {number : int; color : color;} -> number
  |Joker {number : int; color : color;} -> number


let compare_tiles x y = 
  if (get_number x) > (get_number y) then 1 else -1
  *)  
  
  
(* compare function should sort by num first then color*)

let rec add_tile tile row_letter acc = function
  | [] -> failwith "Row not on board."
  | { row = r; visible = v; tiles = ts } :: t ->
      if r = row_letter then
        acc
        @ {
            row = row_letter;
            visible = true;
            (* test sorting later, need to fix adding sort Joker*)
            tiles = List.sort compare (tile :: ts);
          }
          :: t
      else
        add_tile tile row_letter
          (acc @ [ { row = r; visible = v; tiles = ts } ])
          t

(* Need to check remove....*)
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
        remove_tile tile row_letter
          (acc @ [ { row = r; visible = v; tiles = ts } ])
          t

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

  (*acc is true
let rec valid_board acc = function
  | [] -> acc
  | h :: t ->
      valid_board
        ((List.length h = 0 || valid_run h || valid_group h) && acc)
        t
*)
let rec tiles_of_board board = 
  match board with
  | [] -> []
  |h::t -> h.tiles :: (tiles_of_board t)

let rec valid_rows acc tile_rows = 
  match tile_rows with 
  | [] -> acc
  | h :: t ->
      valid_rows((List.length h = 0 || valid_run h || valid_group h) && acc) t

let valid_board acc board = 
  let tile_rows = tiles_of_board board in 
  valid_rows acc tile_rows
