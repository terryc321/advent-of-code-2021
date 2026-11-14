
(* main.ml == module Main *)

let read_file_to_string filename =
  In_channel.with_open_text filename In_channel.input_all

let cwd () = Printf.printf "Current working directory: %s\n" (Sys.getcwd ())  

(* Usage *)
let () =   
   let content = read_file_to_string "../input.txt" in print_endline content

;; let () = print_endline "Hello, World!"


(* Usage *)
let () = Day16.Util.print_int_list [10; 20; 30] (* Output: [10; 20; 30] *)


(* ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F']  *)
let () = let hexchars = Day16.Util.string_to_chars "0123456789ABCDEF" in
List.iter (fun x -> Printf.printf "testing %c => " x ;
	       Day16.Util.print_int_list (Day16.Util.hex_to_bools x)) hexchars


	       (* each letter in string generates a list of booleans , which are eventually all flattened out to a big list of
		  booleans*)	       
	       
let vv : int list =
	       let abcd = Day16.Util.string_to_chars "ABCD" in
	       let fn = (fun x -> Day16.Util.hex_to_bools x) in
	       let xs = List.map fn abcd in
	       List.flatten xs 

	       

	       
	       
	       

