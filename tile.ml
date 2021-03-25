type t =
  | Tile of t_rec
  | Joker of t_rec

(* [t] is a helper for tile type and defines a tile record.*)
and t_rec = {
  number : int;
  color : color;
}

and color =
  | Blue
  | Orange
  | Red
  | Black
  | None

exception NotEnoughTiles

exception NotAJoker

let n_lst = List.init 13 (( + ) 1)

let c_lst = [ Blue; Orange; Red; Black ]

let make_t t n c =
  match t with
  | "T" -> Tile { number = n; color = c }
  | "J" -> Joker { number = n; color = c }
  | _ -> failwith "Not a Tile or Joker"

let joker = make_t "J" 0 None

let update_joker n c = function
  | Joker t -> make_t "J" n c
  | Tile t -> raise NotAJoker

let rec make_tile_aux acc n_lst c =
  match n_lst with
  | [] -> acc
  | n :: ns -> make_tile_aux (make_t "T" n c :: acc) ns c

let make_tile_lst () =
  joker :: joker
  :: List.concat_map (make_tile_aux [] n_lst) (c_lst @ c_lst)

(* Note, this helper function was modified from here:
   https://stackoverflow.com/a/15095713 *)
let shuffle_tile_lst (acc : t list) =
  Random.self_init ();
  let random_int_lst = List.map (fun x -> (Random.int 1000, x)) acc in
  let sorted_int_lst = List.sort compare random_int_lst in
  List.map snd sorted_int_lst

let make_tile_stack () =
  make_tile_lst () |> shuffle_tile_lst |> List.to_seq |> Stack.of_seq

let draw_tile (stack : t Stack.t) =
  try Stack.pop stack with Stack.Empty -> raise NotEnoughTiles

let rec make_rack_aux stack acc = function
  | 0 -> acc
  | i -> make_rack_aux stack (draw_tile stack :: acc) (i - 1)

let make_tile_rack stack =
  if Stack.length stack >= 14 then make_rack_aux stack [] 14
  else raise NotEnoughTiles

let rec numbers_of_t acc = function
  | [] -> List.rev acc
  | Tile t :: tail -> numbers_of_t (t.number :: acc) tail
  | Joker t :: tail -> numbers_of_t (t.number :: acc) tail

let rec colors_of_t acc = function
  | [] -> List.rev acc
  | Tile t :: tail -> colors_of_t (t.color :: acc) tail
  | Joker t :: tail -> colors_of_t (t.color :: acc) tail

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

let rec valid_board acc = function
  | [] -> acc
  | h :: t ->
      valid_board
        ((List.length h = 0 || valid_run h || valid_group h) && acc)
        t
