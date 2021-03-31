type object_phrase = {tiles: string list; row: string}

type command = 
  | Draw
  | Move of object_phrase
  | End
  | Quit

  exception Empty

  exception Malformed



let rec cleanup lst = 
  match lst with
  |[] -> []
  |h::t -> if h = "" then cleanup t else h::cleanup t
let parse str = 
  let strlist = String.split_on_char ' ' (String.lowercase_ascii str) in
    match cleanup strlist with 
    | [] -> raise Empty
    | h::t -> if h = "draw" then if t = [] then Draw else raise Malformed 
      else if h = "move" then if t = [] then raise Malformed else 
        let cmd = String.concat " " t in let length = String.length cmd in 
        Move {tiles = String.split_on_char ' ' (String.sub cmd 0 (length - 5)); 
        row = Char.escaped (String.get cmd (length-1))}
      else if h = "end" then if t = [] then End else raise Malformed
      else if h = "quit" then 
      if t = [] then Quit else raise Malformed else 
    raise Malformed