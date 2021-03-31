open Str

exception BlankInput

exception Malformed

exception NameTooLong

(* type move_phrase = string list *)

type move_phrase = {
  from_board : (string * int) list;
  from_rack : int list;
  to_row : string;
}

type command =
  | Move of move_phrase
  | Undo
  | Reset
  | SortByNumber
  | SortByColor
  | Draw
  | EndTurn
  | Help
  | Quit

let init_two_players = function
  | [] -> [ (1, "Player 1"); (2, "Player 2") ]
  | [ p1 ] -> [ (1, p1); (2, "Player 2") ]
  | [ p1; p2 ] -> [ (1, p1); (2, p2) ]
  | _ -> raise Malformed

let init_four_players = function
  | [] ->
      [
        (1, "Player 1");
        (2, "Player 2");
        (3, "Player 3");
        (4, "Player 4");
      ]
  | [ p1 ] ->
      [ (1, p1); (2, "Player 2"); (3, "Player 3"); (4, "Player 4") ]
  | [ p1; p2 ] -> [ (1, p1); (2, p2); (3, "Player 3"); (4, "Player 4") ]
  | [ p1; p2; p3 ] -> [ (1, p1); (2, p2); (3, p3); (4, "Player 4") ]
  | [ p1; p2; p3; p4 ] -> [ (1, p1); (2, p2); (3, p3); (4, p4) ]
  | _ -> raise Malformed

let trim_lc_fst_word str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str)
  |> List.mapi (fun i x ->
         if i = 0 then String.lowercase_ascii x else x)

let rec check_name_len = function
  | [] -> ()
  | h :: t ->
      if String.length h >= 20 then raise NameTooLong
      else check_name_len t

let parse_start str =
  match trim_lc_fst_word str with
  | "2" :: t ->
      check_name_len t;
      init_two_players t
  | "two" :: t ->
      check_name_len t;
      init_two_players t
  | "4" :: t ->
      check_name_len t;
      init_four_players t
  | "four" :: t ->
      check_name_len t;
      init_four_players t
  | [ "quit" ] ->
      print_string "Thank you for playing! Goodby\n\n.";
      Stdlib.exit 0
  | _ -> raise Malformed

(* let rec get_from_rack board_acc rack_acc = function | [] ->
   {board=to_row = List.filter (fun x -> String.length x = 1); rack =
   rack_acc; bo | h:: t -> try get_from_rack ((int_of_string h) :: acc)
   t with _ -> let parse_move str_lst = let to_removed = List.filter
   (fun x -> x <> "to") str_lst in let rack = List.filter
   (Str.string_match (Str.regexp "[0-9]+$") to_removed 0)

   let cmd = String.concat " " str_lst in let length = String.length cmd
   in let move_rec = Move { tiles = String.split_on_char ' ' (String.sub
   cmd 0 (length - 5)); row = Char.escaped cmd.[length - 1]; } in *)

let rec split_board (acc : (string * int) list) = function
  | [] -> acc
  | h :: t ->
      if String.length h = 2 then
        split_board
          ((String.make 1 h.[0], int_of_string (String.make 1 h.[1]))
          :: acc)
          t
      else if String.length h = 3 then
        split_board
          ((String.make 1 h.[0], int_of_string (String.sub h 1 2))
          :: acc)
          t
      else raise Malformed

let parse_move str_lst =
  try
    let to_removed =
      if List.exists (fun x -> x = "to") str_lst then
        List.filter (fun x -> x <> "to") str_lst
      else raise Malformed
    in
    let from_rack =
      List.filter
        (fun x ->
          Str.string_match (Str.regexp "[0-9]") x 0
          && (String.length x = 1 || String.length x = 2))
        to_removed
      |> List.map (fun x -> int_of_string x)
    in
    let to_row =
      List.hd
        (List.filter
           (fun x ->
             Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?]|}) x 0
             && String.length x = 1)
           to_removed)
    in
    let board =
      List.filter
        (fun x ->
          Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?][1-9]|}) x 0
          && (String.length x = 2 || String.length x = 3))
        to_removed
    in
    let board_split = split_board [] board in
    Move { from_board = board_split; from_rack; to_row }
  with _ -> raise Malformed

let parse str =
  if String.length (String.trim str) = 0 then raise BlankInput
  else
    let str_lst = trim_lc_fst_word str in

    let check_lst = function
      | [ "quit" ] -> Quit
      | [ "move" ] -> raise Malformed
      | "move" :: t -> parse_move t
      | [ "undo" ] -> Undo
      | [ "reset" ] -> Reset
      | [ "color"; "sort" ] -> SortByColor
      | [ "number"; "sort" ] -> SortByNumber
      | [ "draw" ] -> Draw
      | [ "end"; "turn" ] -> EndTurn
      | [ "help" ] -> Help
      | _ -> raise Malformed
    in
    check_lst str_lst

(* type object_phrase = {tiles: string list; row: string}

   type command = | Draw | Move of object_phrase | End | Quit

   exception Empty

   exception Malformed

   let rec cleanup lst = match lst with |[] -> [] |h::t -> if h = ""
   then cleanup t else h::cleanup t let parse str = let strlist =
   String.split_on_char ' ' (String.lowercase_ascii str) in match
   cleanup strlist with | [] -> raise Empty | h::t -> if h = "draw" then
   if t = [] then Draw else raise Malformed else if h = "move" then if t
   = [] then raise Malformed else let cmd = String.concat " " t in let
   length = String.length cmd in Move {tiles = String.split_on_char ' '
   (String.sub cmd 0 (length - 5)); row = Char.escaped (String.get cmd
   (length-1))} else if h = "end" then if t = [] then End else raise
   Malformed else if h = "quit" then if t = [] then Quit else raise
   Malformed else raise Malformed *)
