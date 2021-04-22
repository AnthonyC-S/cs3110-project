(** Need Command Module Description *)

exception BlankInput

exception Malformed

exception NameTooLong

exception NotUniqueNames

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

let check_unique_names name_list =
  if
    List.length name_list
    <> List.length (List.sort_uniq compare name_list)
  then raise NotUniqueNames
  else ()

let parse_start str =
  match trim_lc_fst_word str with
  | "2" :: t | "two" :: t ->
      check_name_len t;
      check_unique_names t;
      init_two_players t
  | "4" :: t | "four" :: t ->
      check_name_len t;
      check_unique_names t;
      init_four_players t
  | [ "q" ] | [ "quit" ] ->
      print_string
        "\n  \027[38;5;70mThank you for playing! Goodbye.\027[0m\n\n";
      Stdlib.exit 0
  | _ -> raise Malformed

let rec split_board acc = function
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

let parse_move_from_board str_lst =
  let board =
    List.filter
      (fun x ->
        Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?][1-9]|}) x 0
        && (String.length x = 2 || String.length x = 3))
      str_lst
  in
  split_board [] board

let parse_move_from_rack str_lst =
  List.filter
    (fun x ->
      Str.string_match (Str.regexp "[0-9]") x 0
      && (String.length x = 1 || String.length x = 2))
    str_lst
  |> List.map (fun x -> int_of_string x)

let parse_move_to_row str_lst =
  List.hd
    (List.filter
       (fun x ->
         Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?]|}) x 0
         && String.length x = 1)
       str_lst)

let parse_move str_lst =
  try
    let rec split_at_to acc = function
      | "to" :: t ->
          let from_board = parse_move_from_board acc in
          let from_rack = parse_move_from_rack acc in
          if List.length from_board = 0 && List.length from_rack = 0
          then raise Malformed
          else
            Move { from_board; from_rack; to_row = parse_move_to_row t }
      | h :: t -> split_at_to (h :: acc) t
      | [] -> raise Malformed
    in
    split_at_to [] str_lst
  with _ -> raise Malformed

let parse str =
  if String.length (String.trim str) = 0 then raise BlankInput
  else
    let str_lst = trim_lc_fst_word str in

    let check_lst = function
      | [ "quit" ] | [ "q" ] | [ "exit" ] -> Quit
      | [ "move" ] -> raise Malformed
      | "move" :: t | "mv" :: t | "play" :: t | "add" :: t ->
          parse_move t
      | [ "undo" ] -> Undo
      | [ "reset" ] -> Reset
      | [ "color"; "sort" ] | [ "sort"; "color" ] | [ "sc" ] ->
          SortByColor
      | [ "number"; "sort" ] | [ "sort"; "number" ] | [ "sn" ] ->
          SortByNumber
      | [ "draw" ] | [ "d" ] -> Draw
      | [ "end"; "turn" ] -> EndTurn
      | [ "help" ] | [ "h" ] -> Help
      | _ -> raise Malformed
    in
    check_lst str_lst
