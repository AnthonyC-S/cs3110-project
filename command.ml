(** Need Command Module Description *)

exception BlankInput

exception Malformed

exception NameTooLong

exception NotUniqueNames

exception InvalidMoveMissingTo

exception EmptyMove

exception EmptyMoveFrom

exception EmptyMoveTo

exception InvalidMoveFrom of string list

exception InvalidMoveTo of string list

exception DuplicateMoveFrom of string list

exception MultipleMoveTo of string list

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
      else
        split_board
          ((String.make 1 h.[0], int_of_string (String.sub h 1 2))
          :: acc)
          t

let valid_board_syntax s =
  Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?][1-9]|}) s 0
  && (String.length s = 2 || String.length s = 3)

let valid_rack_syntax s =
  Str.string_match (Str.regexp "[0-9]") s 0
  && (String.length s = 1 || String.length s = 2)

let rec check_for_dups = function
  | [] -> []
  | h :: t ->
      if List.mem h t then h :: check_for_dups t else check_for_dups t

let parse_from str_lst =
  let f_board = List.filter valid_board_syntax str_lst in
  let f_rack = List.filter valid_rack_syntax str_lst in
  let f_malformed =
    List.filter (fun s -> not (List.mem s (f_board @ f_rack))) str_lst
  in
  let f_dups = List.sort_uniq compare (check_for_dups str_lst) in
  (f_board, f_rack, f_malformed, f_dups)

let valid_to_row_syntax s =
  Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?]|}) s 0
  && String.length s = 1

let parse_to str_lst =
  let to_row = List.filter valid_to_row_syntax str_lst in
  let to_malformed =
    List.filter (fun s -> not (List.mem s to_row)) str_lst
  in
  (to_row, to_malformed)

let parse_rack_and_board before_to_lst after_to_lst =
  let f_board, f_rack, f_malformed, f_dups = parse_from before_to_lst in
  let to_row, to_malformed = parse_to after_to_lst in
  if f_malformed <> [] then raise (InvalidMoveFrom f_malformed)
  else if f_dups <> [] then raise (DuplicateMoveFrom f_dups)
  else if f_board = [] && f_rack = [] then raise EmptyMoveFrom
  else if to_malformed <> [] then raise (InvalidMoveTo to_malformed)
  else if List.length to_row > 1 then raise (MultipleMoveTo to_row)
  else if to_row = [] then raise EmptyMoveTo
  else
    Move
      {
        from_board = split_board [] f_board;
        from_rack = List.map (fun x -> int_of_string x) f_rack;
        to_row = List.hd to_row;
      }

let rec parse_move acc = function
  | "to" :: t | "TO" :: t -> parse_rack_and_board (List.rev acc) t
  | h :: t -> parse_move (h :: acc) t
  | [] -> raise InvalidMoveMissingTo

let parse str =
  if String.length (String.trim str) = 0 then raise BlankInput
  else
    let str_lst = trim_lc_fst_word str in
    let check_lst = function
      | [ "quit" ] | [ "q" ] | [ "exit" ] -> Quit
      | [ "move" ] | [ "mv" ] | [ "m" ] | [ "play" ] | [ "add" ] ->
          raise EmptyMove
      | "move" :: t | "mv" :: t | "m" :: t | "play" :: t | "add" :: t ->
          parse_move [] (List.map (fun s -> String.uppercase_ascii s) t)
      | [ "undo" ] | [ "u" ] -> Undo
      | [ "reset" ] | [ "r" ] -> Reset
      | [ "color"; "sort" ] | [ "sort"; "color" ] | [ "sc" ] ->
          SortByColor
      | [ "number"; "sort" ] | [ "sort"; "number" ] | [ "sn" ] ->
          SortByNumber
      | [ "draw" ] | [ "d" ] -> Draw
      | [ "end"; "turn" ] | [ "et" ] -> EndTurn
      | [ "help" ] | [ "h" ] -> Help
      | _ -> raise Malformed
    in
    check_lst str_lst
