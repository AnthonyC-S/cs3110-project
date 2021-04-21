open Tile

type b_row = {
  row : string;
  tiles : t list;
}

type b = b_row list

exception NotValidBoardRow

exception RowAlreadyFull

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

(* assign_joker_from_board
assign_joker_in_rack *)


let rec color_filter acc list =
  match list with 
  | []-> acc
  | h::t -> if ((List.find_opt (fun x -> x == h) list) != None ) 
    then color_filter (List.filter (fun x -> x <> h) acc) t 
    else  color_filter acc t
(* Returns None if row already has every color*)
let rec find_color ts = 
  let colorlist = colors_of_t [] ts in
  let colors = [ Black; Red; Blue; Orange; None ] in
  let result = color_filter colors colorlist in
  match result with
  | [] -> raise NotValidBoardRow
  | h::t -> h
(* Has the Tile.NotAJoker if out of bounds *)
let rec find_num fst tile ts = 
  match ts with
  | [] -> get_tile_number tile
  | [ h ] ->  if (get_tile_number h) >= 13 then fst - 1 else get_tile_number h + 1
  | [h; h2] -> if get_tile_number h2 - get_tile_number h = 1 then
    if (get_tile_number h2) >= 13 then fst - 1 else get_tile_number h2 + 1
    else  get_tile_number h + 1
  | h::h2::t -> if get_tile_number h2 - get_tile_number h = 1 then find_num fst tile (h2::t) else get_tile_number h + 1

  (* assumes that it is a valid move. If not, it will still be caught by checking valid_board*)
let new_joker tile ts = 
  let set = (List.filter (fun x -> x <> tile) ts)@[Joker { number = 1; color = None}] in
  let fst = get_tile_number (List.hd set) in
  match set with
  | [] -> tile
  | [ h ] -> update_joker (find_num fst tile set) (get_tile_color h) tile
  | [h; h2] -> if get_tile_color h = get_tile_color h2 then update_joker (find_num fst tile set) (get_tile_color h) tile
  else update_joker (fst) (find_color set) tile
  | h::h2::t -> if get_tile_color h = get_tile_color h2 then update_joker (find_num fst tile set) (get_tile_color h) tile
  else update_joker (fst) (find_color set) tile 

let rec check_joker acc st orig = 
  match st with 
  | [] -> List.rev acc
  | Joker t:: tail -> check_joker ((new_joker (Joker t) orig)::acc) tail orig
  | Tile t :: tail -> check_joker ((Tile t):: acc) tail orig
let updated_row st =
  let joker_row = sort_by_number st.tiles in 
  let new_tiles = sort_by_number (check_joker [] joker_row joker_row) in
  {row= st.row; tiles= new_tiles}

let rec add_tile tile row_letter acc = function
  | [] -> raise NotValidBoardRow
  | { row = r; tiles = ts } :: t ->
      if r = row_letter && List.length ts == 13 then
        raise RowAlreadyFull
      else if r = row_letter && List.length ts < 13 then
        acc @ ({ row = row_letter; tiles = tile :: ts } :: t)
      else
        add_tile tile row_letter (acc @ [ { row = r; tiles = ts } ]) t

(* [replace_tile_by_index tile row_letter acc index st.current_board]
   replaces the old tile with a new [tile] in [st.current_board] at
   [row_letter] and at the [index]. Specifically, this is used to assign
   Jokers a number or color, no other tiles should ever need to be
   replaced. *)
let rec replace_tile_by_index tile row_letter acc index = function
  | [] -> raise NotValidBoardRow
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
      else
        add_tile tile row_letter (acc @ [ { row = r; tiles = ts } ]) t


let rec remove_tile tile row_letter acc = function
  | [] -> raise NotValidBoardRow
  | { row = r; tiles = ts } :: t ->
      if r = row_letter then
        acc
        @ {
            row = row_letter;
            tiles = List.filter (fun x -> x <> tile) ts;
          }
          :: t
      else
        remove_tile tile row_letter
          (acc @ [ { row = r; tiles = ts } ])
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
