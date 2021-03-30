exception BlankInput

exception Malformed

exception NameTooLong

type move_phrase = string list

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

let parse_move str_lst = failwith "parse_move Need To Do"

let parse str =
  if String.length (String.trim str) = 0 then raise BlankInput
  else
    let str_lst = trim_lc_fst_word str in

    let check_lst = function
      | [ "quit" ] -> Quit
      | "quit" :: t -> raise Malformed
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
