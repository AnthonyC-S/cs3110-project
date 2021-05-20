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
  | Score
  | Help
  | Quit

(** [init_two_players slst] is a pair (k, v) list [pinfo_lst] with [k]
    being the number of the player and [v] being the name of the player.
    The length of [pinfo_lst] is 2 since this function is called when
    there are 2 players. [slst] is the list of the names of players the
    user entered. If [slst] does not have element with index [i], the
    name of the player with the same number as [i] is set the "Player
    [i]". The turn order (= player numbers) are the same as the order of
    names in [slst]. *)
let init_two_players = function
  | [] -> [ (1, "Player 1"); (2, "Player 2") ]
  | [ p1 ] -> [ (1, p1); (2, "Player 2") ]
  | [ p1; p2 ] -> [ (1, p1); (2, p2) ]
  | _ -> raise Malformed

(** [init_three_players slst] is the same as [init_two_players] function
    but the length of the [pinfo_lst] is 3 since this function is called
    when there are three players. *)
let init_three_players = function
  | [] -> [ (1, "Player 1"); (2, "Player 2"); (3, "Player 3") ]
  | [ p1 ] -> [ (1, p1); (2, "Player 2"); (3, "Player 3") ]
  | [ p1; p2 ] -> [ (1, p1); (2, p2); (3, "Player 3") ]
  | [ p1; p2; p3 ] -> [ (1, p1); (2, p2); (3, p3) ]
  | _ -> raise Malformed

(** [init_four_players slst] is the same as [init_two_players] function
    but the length of the [pinfo_lst] is 4 since this function is called
    when there are four players. *)
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

(** [trim_lc_fst_word s] is string list [slst] with each element being
    words from [s]. The first element in [slst] is made lowercase. *)
let trim_lc_fst_word str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str)
  |> List.mapi (fun i x ->
         if i = 0 then String.lowercase_ascii x else x)

(** [trim_lc_lowercase s] is the same as [trim_lc_fst_word] but with all
    elements of [slst] in lowercase *)
let trim_lc_lowercase str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str)
  |> List.map (fun x -> String.lowercase_ascii x)

(** [check_name_len slst] checks if the length of all the strings in
    [slst] is under 20 and returns [()]. If not, [NameTooLong] is
    raised. *)
let rec check_name_len = function
  | [] -> ()
  | h :: t ->
      if String.length h >= 20 then raise NameTooLong
      else check_name_len t

(** [check_unique_names lst] checks if all the elements in [lst] are
    unique and returns [()]. If not, [NotUniqueNames] is raised. *)
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
  | "3" :: t | "three" :: t ->
      check_name_len t;
      check_unique_names t;
      init_three_players t
  | "4" :: t | "four" :: t ->
      check_name_len t;
      check_unique_names t;
      init_four_players t
  | [ "q" ] | [ "quit" ] ->
      print_string
        "\n  \027[38;5;70mThank you for playing! Goodbye.\027[0m\n\n";
      Stdlib.exit 0
  | _ -> raise Malformed

(** [split_board acc slst] is a ([k], [v]) pair list [bt_lst] where [k]
    is the row letter on the board and [v] is the index location of the
    tile selected. *)
let rec split_board acc = function
  | [] -> List.rev acc
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

(** [valid_board_syntax s] is true if format of [s] is a concatentation
    of a letter among alphabets or !\@#$%^&? and one or more number
    digits. [s] should also have a length either 2 or 3. *)
let valid_board_syntax s =
  Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?][1-9]|}) s 0
  && (String.length s = 2 || String.length s = 3)

(** [valid_rack_syntax s] is true if format of [s] is concatenation of 1
    or 2 number digits. *)
let valid_rack_syntax s =
  Str.string_match (Str.regexp "^[1-9][0-9]?$") s 0
  && (String.length s = 1 || String.length s = 2)

(** [check_for_dups lst] is a list [lst'] that contains the elements
    that has duplicates in [lst]. Duplicates are allowed in [lst']. *)
let rec check_for_dups = function
  | [] -> []
  | h :: t ->
      if List.mem h t then h :: check_for_dups t else check_for_dups t

(** [parse_from slst] is [(l1, l2, l3, l4)] where [l1] is string list of
    references to tiles on the board with the correct format. [l2] is
    string list of references to tiles in rack with correct format. [l3]
    contains strings that doesn't match both formats for board tiles and
    rack tiles. [l4] is string list of references that were not unique
    in [slst]. *)
let parse_from str_lst =
  let f_board = List.filter valid_board_syntax str_lst in
  let f_rack = List.filter valid_rack_syntax str_lst in
  let f_malformed =
    List.filter (fun s -> not (List.mem s (f_board @ f_rack))) str_lst
  in
  let f_dups = List.sort_uniq compare (check_for_dups str_lst) in
  (f_board, f_rack, f_malformed, f_dups)

(** [valid_to_row_syntax s] is true if [s] is one letter among alphabets
    or !\@#$%^&?. *)
let valid_to_row_syntax s =
  Str.string_match (Str.regexp {|[a-zA-Z!@#\$%\^&\?]|}) s 0
  && String.length s = 1

(** [parse_to slst] is [(l1, l2)] where [l1] is the row name and [l2] is
    strings in [slst] that don't have a valid row name format. *)
let parse_to str_lst =
  let to_row = List.filter valid_to_row_syntax str_lst in
  let to_malformed =
    List.filter (fun s -> not (List.mem s to_row)) str_lst
  in
  (to_row, to_malformed)

(** [parse_rack_and_board bto_lst ato_lst] is a [Move info] command. If
    [bto_lst] contains malformed references to tiles,
    [InvalidMoveFrom tlst1] is raised where [tlst1] is a list of the
    maformed tile references. I [bto_lst] contains duplicate references
    to the same tile, [DuplicateMoveFrom tlst2] is raised where [tlst2]
    is a list of the duplicate tile references . If [bto_lst] doesn't
    contain any references to tiles, [EmptyMoveFrom] is raised. If
    [ato_lst] contains malfored references to a row,
    [InvalidMoveTo rlst1] where [rlst1] is a list of the malformed row
    reference. If [ato_lst] contains more than 1 reference to a row,
    [MultipleMoveTo rlst2] is raised where [rlst2] is the row
    references. If [ato_lst] does not contain any reference to a row,
    [EmptyMoveTo] is raised. *)
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
        from_board =
          split_board [] (List.map String.uppercase_ascii f_board);
        from_rack =
          List.map
            (fun x -> int_of_string x)
            (List.map String.uppercase_ascii f_rack);
        to_row = List.hd (List.map String.uppercase_ascii to_row);
      }

(** [parse_move acc slst] is a [Move info] command with tile references
    and row letter the tiles are being moved to from [slst]. *)
let rec parse_move acc = function
  | "to" :: t | "To" :: t | "TO" :: t | "tO" :: t ->
      parse_rack_and_board (List.rev acc) t
  | h :: t -> parse_move (h :: acc) t
  | [] -> raise InvalidMoveMissingTo

let parse str =
  if String.length (String.trim str) = 0 then raise BlankInput
  else
    let str_lst = trim_lc_lowercase str in
    let check_lst = function
      | [ "quit" ] | [ "q" ] | [ "exit" ] -> Quit
      | [ "move" ] | [ "mv" ] | [ "m" ] -> raise EmptyMove
      | "move" :: t | "mv" :: t | "m" :: t -> parse_move [] t
      | [ "undo" ] | [ "u" ] -> Undo
      | [ "reset" ] | [ "r" ] -> Reset
      | [ "colorsort" ] | [ "sortcolor" ] | [ "sc" ] -> SortByColor
      | [ "numbersort" ] | [ "sortnumber" ] | [ "sn" ] -> SortByNumber
      | [ "draw" ] | [ "d" ] -> Draw
      | [ "endturn" ] | [ "e" ] -> EndTurn
      | [ "score" ] | [ "s" ] -> Score
      | [ "help" ] | [ "h" ] -> Help
      | _ -> raise Malformed
    in
    check_lst str_lst
