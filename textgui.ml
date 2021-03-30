open Tile
open Board
open State

let stack_size s = s.t_stack |> tile_stack_size |> string_of_int

let pile_string s =
  let stack_size_str = stack_size s in
  match String.length stack_size_str with
  | 1 -> stack_size_str ^ "         "
  | 2 -> stack_size_str ^ "        "
  | 3 -> stack_size_str ^ "       "
  | _ -> stack_size_str ^ "          "

let top_string s =
  " \
   ________________________________________________________________________________________________________\n"
  ^ " \
     |                                                                             \
     Current Turn: Player "
  ^ string_of_int s.current_turn
  ^ "    |\n"
  ^ " \
     |                                                                                     \
     Pile: " ^ pile_string s ^ "  |\n"
  ^ " | Index:  1  2  3  4  5  6  7  8  9  10  11  12  13  ||      1  \
     2  3  4  5  6  7  8  9  10  11  12  13  | \n"
  ^ " \
     |-------------------------------------------------------------------------------------------------------|\n"

let print_top s = ANSITerminal.print_string [] (top_string s)

let rec match_color_ANSI acc = function
  | [] -> List.rev acc
  | Black :: t -> match_color_ANSI (ANSITerminal.black :: acc) t
  | Red :: t -> match_color_ANSI (ANSITerminal.red :: acc) t
  | Blue :: t -> match_color_ANSI (ANSITerminal.blue :: acc) t
  | Orange :: t -> match_color_ANSI (ANSITerminal.green :: acc) t
  | None :: t -> match_color_ANSI (ANSITerminal.default :: acc) t

let num_space str n =
  let str_l = String.length str in
  let spaces = String.make (n - str_l) ' ' in
  spaces

(** [space_t tstr i] is a string [tstr'] that has spaces concatenated at
    the end [tstr]. If the index number of tile [t]'s string
    representation [tstr] is < 9, then the length of [tstr'] is 3. the
    length of [tstr'] is 4. *)
let space_t tstr i =
  if i < 9 then num_space tstr 3 else num_space tstr 4

let print_tile tstr c i =
  if String.trim tstr = "" then ANSITerminal.print_string [] tstr
  else (
    ANSITerminal.print_string [ ANSITerminal.on_white; c ] tstr;
    ANSITerminal.print_string [] (space_t tstr i))

let row_color_list slst r =
  if List.hd slst = "   " then
    List.init 13 (fun _ -> ANSITerminal.default)
  else
    let colors = match_color_ANSI [] (colors_of_t [] r.tiles) in
    let clst_l = List.length colors in
    if clst_l < 13 then
      colors @ List.init (13 - clst_l) (fun _ -> ANSITerminal.default)
    else colors

let row_num_style rname n =
  if (n - 1) mod 2 = 0 then (" |     " ^ rname ^ ":  ", "||  ")
  else (rname ^ ":  ", "|\n")

(** prints each line with 2 rows. Length of [slst] and [clst] should be
    the same 13. *)
let rec print_row_line r n =
  let slst = tiles_of_string_lst [] r.tiles
  and row_style = row_num_style r.row n in
  let clst = row_color_list slst r in
  ANSITerminal.print_string [] (fst row_style);
  for i = 0 to 12 do
    print_tile (List.nth slst i) (List.nth clst i) i
  done;
  ANSITerminal.print_string [] (snd row_style)

let rec number_each_lst_ele n acc = function
  | [] -> List.rev acc
  | h :: t ->
      let new_acc = (n, h) :: acc in
      number_each_lst_ele (n + 1) new_acc t

let rec print_row_lines rs =
  match rs with
  | [] -> ()
  | h :: t ->
      print_row_line (snd h) (fst h);
      print_row_lines t

let print_rows s =
  let numbered_rows = number_each_lst_ele 1 [] s.current_board in
  print_row_lines numbered_rows

(* use visible field of b_row to decide which rows to show: rows with
   tiles & 2 more empty rows *)

let print_bottom () =
  ANSITerminal.print_string []
    " \
     |_______________________________________________________________________________________________________|"

let print_state s =
  print_top s;
  print_rows s;
  print_bottom ()
