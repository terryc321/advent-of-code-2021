
(* convert hex character to booleans *)

exception InvalidHexCharacter

let hex_to_bools c =
  match c with
  | '0' -> [0;0;0;0]
  | '1' -> [0;0;0;1]
  | '2' -> [0;0;1;0]
  | '3' -> [0;0;1;1]
  | '4' -> [0;1;0;0]
  | '5' -> [0;1;0;1]
  | '6' -> [0;1;1;0]
  | '7' -> [0;1;1;1]
  | '8' -> [1;0;0;0]
  | '9' -> [1;0;0;1]
  | 'A' -> [1;0;1;0]
  | 'B' -> [1;0;1;1]
  | 'C' -> [1;1;0;0]
  | 'D' -> [1;1;0;1]
  | 'E' -> [1;1;1;0]
  | 'F' -> [1;1;1;1]
  | _   -> raise InvalidHexCharacter


let print_int_list lst =
  print_string "[";
  List.iteri (fun i x ->
    if i > 0 then print_string "; ";
    Printf.printf "%d" x
  ) lst;
  print_endline "]"

let string_to_chars s =
  s |> String.to_seq |> List.of_seq        


let hex2bools (s : string) : int list =
	       let abcd = string_to_chars s in
	       let fn = (fun x -> hex_to_bools x) in
	       let xs = List.map fn abcd in
	       List.flatten xs 
