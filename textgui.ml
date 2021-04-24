open State
open Player
open Tile

(* [rp acc str i] concatanates [str] [i] number of times. *)
let rec rp acc str = function
  | 0 -> acc
  | i -> rp (str ^ acc) str (i - 1)

(* black on white *)
let kw s = "\027[38;5;0;1m\027[48;5;15;1m" ^ s ^ "\027[0m\027[0m"

(* red on white *)
let rw s = "\027[38;5;9;1m\027[48;5;15;1m" ^ s ^ "\027[0m"

(* orange on white *)
let ow s = "\027[38;5;208;1m\027[48;5;15;1m" ^ s ^ "\027[0m"

(* blue on white*)
let bw s = "\027[38;5;26;1m\027[48;5;15;1m" ^ s ^ "\027[0m"

(* italic green *)
let ig s = "\027[38;5;70;3m" ^ s ^ "\027[0m"

(* regular green *)
let g s = "\027[38;5;70m" ^ s ^ "\027[0m"

(* [ip] is short for input and is frequently used before a read_line. *)
let ip : string = g "  > "

let top_row = " " ^ String.make 105 '_' ^ " \n"

let center_str str =
  let str_len = String.length str in
  let spaces_needed = 103 - str_len in
  let space = String.make (spaces_needed / 2) ' ' in
  if spaces_needed mod 2 = 0 then " |" ^ space ^ str ^ space ^ "|\n"
  else " |" ^ space ^ str ^ space ^ " |\n"

let empty_row = center_str ""

let turn_row cur_player =
  " |  " ^ g "Turn:  " ^ cur_player
  ^ String.make (94 - String.length cur_player) ' '
  ^ "|\n"

let meld_row cur_player =
  " |  " ^ g "Meld:  "
  ^
  if cur_player.played_valid_meld then
    "Met" ^ String.make 91 ' ' ^ "|\n"
  else "Not Met" ^ String.make 87 ' ' ^ "|\n"

let pile_row pile_size =
  let pile_size_str = string_of_int pile_size in
  " |  " ^ g "Pile:  " ^ pile_size_str
  ^
  if pile_size < 10 then String.make 93 ' ' ^ "|\n"
  else String.make 92 ' ' ^ "|\n"

let top_index_r =
  " | " ^ g "Index:"
  ^ "  1  2  3  4  5  6  7  8  9  10  11  12  13  ||      1  2  3  4  \
     5  6  7  8  9  10  11  12  13  |\n"

let dash_row = " |" ^ String.make 103 '-' ^ "|\n"

let bottom_row = " |" ^ String.make 103 '_' ^ "|\n\n"

(* [rack_index_r t_lst] gives the rack index string that is equal in
   length to size of rack [t_lst]. Accounts for spacing needed between
   numbers single digit numbers vs. double digit numbers. *)
let rec rack_index_r t_lst =
  g "  Index:  "
  ^ (List.init (List.length t_lst) (( + ) 1)
    |> List.map (fun i -> string_of_int i ^ "  ")
    |> String.concat "")
  ^ "\n"

let welcome_msg =
  " |" ^ String.make 28 ' '
  ^ ig " 🐫 WELCOME TO THE  "
  ^ rw "C" ^ " " ^ kw "A" ^ " " ^ ow "M" ^ " " ^ bw "L" ^ " " ^ kw "K"
  ^ " " ^ ow "U" ^ " " ^ bw "B" ^ ig "  GAME  🐫   "
  ^ String.make 29 ' ' ^ "|\n" ^ empty_row ^ " |" ^ String.make 38 ' '
  ^ ig "Based on the game Rummikub"
  ^ String.make 39 ' ' ^ "|\n"

let developed_by_msg =
  " |" ^ String.make 20 ' '
  ^ g "Developed by Anthony Coffin-Schmitt, Mina Huh, and Christy Song"
  ^ String.make 20 ' ' ^ "|\n |" ^ String.make 39 ' '
  ^ g "For CS 3110, Spring 2021"
  ^ String.make 40 ' ' ^ "|\n"

let welcome_board =
  top_row ^ rp "" empty_row 6 ^ welcome_msg ^ rp "" empty_row 4
  ^ developed_by_msg ^ rp "" empty_row 4 ^ bottom_row

let clear_board () =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 110 45;
  ANSITerminal.set_cursor 1 1

(* [digit_len i] is a helper for [space] and gives the digit length of
   tile numbers. Note that Jokers are initially assigned a number of
   100, but "J" is only a length of 1. *)
let digit_len i = if i = 10 || i = 11 || i = 12 || i = 13 then 2 else 1

(*[space i idx_count] gives the correct number of spaces of the tile
  number when priting the board rows and rack. Takes into account the
  length of the tile number [i] and the index position of where the tile
  is being printed to with [idx_count].*)
let space i idx_count =
  let digit_len = digit_len i in
  if digit_len = 1 && idx_count > 9 then "   "
  else if digit_len = 2 && idx_count < 10 then " "
  else "  "

let string_of_tile idx_count tile =
  match tile with
  | Tile { number = i; color = Blue } ->
      bw (string_of_int i) ^ space i idx_count
  | Tile { number = i; color = Orange } ->
      ow (string_of_int i) ^ space i idx_count
  | Tile { number = i; color = Red } ->
      rw (string_of_int i) ^ space i idx_count
  | Tile { number = i; color = Black } ->
      kw (string_of_int i) ^ space i idx_count
  | Tile { number = i; color = None } -> ""
  | Joker { number = i } -> kw "J" ^ space i idx_count

let rec string_of_tiles acc idx_count tiles =
  match tiles with
  | [] -> acc ^ "\027[0m"
  | h :: t ->
      string_of_tiles
        (acc ^ string_of_tile idx_count h)
        (idx_count + 1) t

(* [get_spaces t_lst] is the number of spaces needed to give a total
   string length of 43 for each row of the board. Note tile indexes < 9
   use 3 characters each and indexes > 9 use 4 characters each, i.e. 9*3
   + 4*4 = 43. *)
let get_spaces t_lst : int =
  let lst_len = List.length t_lst in
  if lst_len < 10 then 43 - (lst_len * 3)
  else 43 - (27 + ((lst_len - 9) * 4))

let rec string_of_board_rows acc (board : Board.b_row list) =
  match board with
  | [] -> acc
  | { row = r1; tiles = t1 } :: { row = r2; tiles = t2 } :: t ->
      string_of_board_rows
        (acc ^ " |     " ^ r1 ^ ":  " ^ string_of_tiles "" 1 t1
        ^ String.make (get_spaces t1) ' '
        ^ "||  " ^ r2 ^ ":  " ^ string_of_tiles "" 1 t2
        ^ String.make (get_spaces t2) ' '
        ^ "|\n")
        t
  | [ { row = r1; tiles = t1 } ] ->
      string_of_board_rows
        (acc ^ " |     " ^ r1 ^ ":  " ^ string_of_tiles "" 1 t1 ^ "\n")
        []

let build_board st msg =
  let cur_player = get_current_player st in
  let turn_row = turn_row cur_player.name in
  let meld_row = meld_row cur_player in
  let pile_row = pile_row (Stack.length st.t_stack) in
  let cur_rack = get_current_rack st.current_turn st.players in
  top_row ^ turn_row ^ meld_row ^ pile_row ^ dash_row ^ top_index_r
  ^ string_of_board_rows "" st.board
  ^ bottom_row ^ rack_index_r cur_rack ^ g "   Rack:  "
  ^ string_of_tiles "" 1 cur_rack
  ^ "\n\n" ^ msg ^ ip
