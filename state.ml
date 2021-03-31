open Player
open Board
open Tile

type s = {
  current_turn : int;
  current_board : b;
  players : p list;
  past_boards : b list;
  t_stack : Tile.t Stack.t;
}

let init_state player_lst =
  let stack = Tile.make_tile_stack () in
  {
    current_turn = 1;
    current_board = init_board ();
    players = make_players [] stack player_lst;
    past_boards = [];
    t_stack = stack;
  }

exception NotValidIndex

exception HaveNotPlayedMeld

exception InvalidBoardSets

exception InvalidMeld

type result =
  | Legal of s
  | Illegal

let update_past_boards st = st.past_boards @ [ st.current_board ]

(* helper for undo_past_move and reset_current_board*)
let get_fst_elm = function
  | h :: t -> h
  | [] -> failwith "Empty past_boards list"

let get_cur_player st = current_player st.current_turn st.players

let get_other_players st =
  List.filter (fun x -> x <> get_cur_player st) st.players

let undo_move st =
  let cur_player = get_cur_player st in
  let last_b = get_fst_elm (List.rev st.past_boards) in
  let last_r = get_fst_elm (List.rev cur_player.past_racks) in
  let last_meld_count = get_fst_elm (List.rev cur_player.meld_count) in
  let last_past_meld_counts =
    get_fst_elm (List.rev cur_player.past_meld_counts)
  in
  let new_player =
    {
      cur_player with
      past_racks =
        List.filter (fun x -> x <> last_r) cur_player.past_racks;
      rack = get_fst_elm (List.rev cur_player.past_racks);
      meld_count =
        List.filter
          (fun x -> x <> last_meld_count)
          cur_player.meld_count;
      past_meld_counts =
        List.filter
          (fun x -> x <> last_past_meld_counts)
          cur_player.past_meld_counts;
    }
  in
  {
    st with
    current_board = last_b;
    past_boards = List.filter (fun x -> x <> last_b) st.past_boards;
    players = new_player :: get_other_players st;
  }

(* [reset_turn st] is the initial game state of [st] at the start of the
   current player's turn. Sets the fields [past_boards] to empty, the
   board to the initial state at the start of the turn, and the current
   players rack to the tiles at the start of the turn. Need to also
   reset player rack. *)
let reset_turn st =
  let cur_player = get_cur_player st in
  let new_player =
    {
      cur_player with
      rack = get_fst_elm cur_player.past_racks;
      past_racks = [];
      meld_count = [];
      past_meld_counts = [];
    }
  in
  {
    st with
    current_board = get_fst_elm st.past_boards;
    past_boards = [];
    players = new_player :: get_other_players st;
  }

(* Need to update past board moves and past rack moves. *)
let move_from_rack st index row =
  let cur_player = get_cur_player st in
  let rack = cur_player.rack in
  let rack_len = List.length rack in
  let tile_to_move = get_tile_of_index index rack in
  if rack_len <> 0 && index - 1 < rack_len && index - 1 >= 0 then
    let new_player =
      {
        cur_player with
        meld_count = tile_to_move :: cur_player.meld_count;
        past_meld_counts =
          cur_player.past_meld_counts @ [ cur_player.meld_count ];
      }
      |> update_past_rack
    in
    {
      st with
      current_board = add_tile tile_to_move row [] st.current_board;
      past_boards = update_past_boards st;
      players =
        new_player :: get_other_players st
        |> remove_from_rack st.current_turn index;
    }
  else raise NotValidIndex

(** helper *)
let rec get_current_player cp (plst : p list) =
  match plst with
  | [] -> []
  | h :: t -> if h.number = cp then h.rack else get_current_player cp t

(*Note, had to do the sort list and reverse order so the higher index
  tiles would be moved first and not mess up the index order other
  tiles. *)
let rec multiple_moves_from_rack st index_lst row =
  match List.rev (List.sort compare index_lst) with
  | [] ->
      { st with current_board = sort_board_by_num [] st.current_board }
  | h :: t -> multiple_moves_from_rack (move_from_rack st h row) t row

(* Note, main.ml needs to check if there are any tiles for all moves
   from rack and assign jokers tiles if they are included. If there are
   jokers in the rack then this function needs to be called before the
   [multiple_moves_from_rack]*)
let assign_joker_in_rack (st : s) (n : int) (c : color) (index : int) =
  let cur_player = get_cur_player st in
  let rack = cur_player.rack in
  let new_rack =
    List.filteri (fun i _ -> i < index - 1) rack
    @ [ update_joker n c (get_tile_of_index index rack) ]
    @ List.filteri (fun i _ -> i > index - 1) rack
  in
  let new_player = { cur_player with rack = new_rack } in
  { st with players = new_player :: get_other_players st }

let move_from_board st from_row index to_row =
  let cur_player = get_cur_player st in
  if cur_player.played_valid_meld = false then raise HaveNotPlayedMeld
  else
    let tile =
      get_tile_of_index index
        (List.find (fun { row = x } -> x = from_row) st.current_board)
          .tiles
    in
    let new_board =
      remove_tile tile from_row [] st.current_board
      |> add_tile tile to_row []
    in
    {
      st with
      current_board = new_board;
      past_boards = update_past_boards st;
      players = update_past_rack cur_player :: get_other_players st;
    }

(* Note, [multiple_moves_from_board] needs to have the board tiles to
   move come in as an association list set up as (index, from_row) list. *)
let rec multiple_moves_from_board st from_lst to_row =
  match List.rev (List.sort compare from_lst) with
  | [] ->
      { st with current_board = sort_board_by_num [] st.current_board }
  | (i, r) :: t ->
      multiple_moves_from_board (move_from_board st r i to_row) t to_row

(* Note, main.ml needs to check if there are any tiles for all moves
   from the board and assign jokers tiles if they are included. If there
   are jokers being moved from the board then this function needs to be
   called before the [multiple_moves_from_board]. Also, [from] is given
   as (index int, row string). *)
let assign_joker_from_board st n c from =
  let tile_to_update =
    get_tile_of_index (fst from)
      (List.find (fun { row = x } -> x = snd from) st.current_board)
        .tiles
  in
  let new_board =
    remove_tile tile_to_update (snd from) [] st.current_board
    |> add_tile_by_index
         (update_joker n c tile_to_update)
         (snd from) [] (fst from)
  in
  { st with current_board = new_board }

(* Note, I did not update past_moves or past rack lists since this will
   be automatically followed by [reset_turn] and then [end_turn] via
   main.ml. *)
let draw (st : s) : s =
  {
    st with
    players =
      add_to_rack st.current_turn st.players (draw_tile st.t_stack);
  }

let sort_rack_by_color st =
  let cur_player = get_cur_player st in
  let new_player =
    { cur_player with rack = sort_by_color cur_player.rack }
  in
  { st with players = new_player :: get_other_players st }

let sort_rack_by_number st =
  let cur_player = get_cur_player st in
  let new_player =
    { cur_player with rack = sort_by_number cur_player.rack }
  in
  { st with players = new_player :: get_other_players st }

let check_valid_board st = valid_board st.current_board

let update_current_turn st =
  (st.current_turn mod List.length st.players) + 1

let end_turn (st : s) : s =
  if not (check_valid_board st) then raise InvalidBoardSets
  else if not (check_for_valid_meld (get_cur_player st)) then
    raise InvalidMeld
  else
    {
      st with
      past_boards = [];
      players =
        (empty_past_rack (get_cur_player st) |> update_played_valid_meld)
        :: get_other_players st;
      current_turn = update_current_turn st;
    }
