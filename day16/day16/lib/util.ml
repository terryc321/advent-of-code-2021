
(* some basic io to read the puzzle file which we call content *)

let chop_newline s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then
    if len > 1 && s.[len - 2] = '\r' then
      String.sub s 0 (len - 2) (* handles Windows \r\n *)
    else
      String.sub s 0 (len - 1) (* handles Unix \n *)
  else
    s

let read_file_to_string filename =
  let str = In_channel.with_open_text filename In_channel.input_all in
  chop_newline str

(* reading text file leaves a newline in the input , so we chopped that out *)

(* current working directory in case we get into trouble *)
let cwd () = Printf.printf "Current working directory: %s\n" (Sys.getcwd ())  

(* Usage *)
let content = read_file_to_string "../input.txt" 

let show_content () = print_endline content


(* convert hex character to booleans *)

exception InvalidHexCharacter of char

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
  | _   -> raise (InvalidHexCharacter c)


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

(* hex2bools "ABCDEF0123456789"*)
(* hex2bools content *)

(* 
3 bool of packet version as number 
3 bool of packet id as number 
we need to keep passing what must be computed next as another parameter of a tuple

we can read packet version
*)

exception BadPacketVersion
exception BadPacketId

let packet_version (xs : int list) : int * int list = 
  match xs with 
  | a :: b :: c :: tl ->  (c + (b * 2) + (a * 4) , tl)
  | _ -> raise BadPacketVersion


let packet_id (xs : int list) : int * int list = 
  match xs with 
  | a :: b :: c :: tl ->  (c + (b * 2) + (a * 4) , tl)
  | _ -> raise BadPacketId

(* peel off 3 bool values and always have the rest of the tail passed back 
this way we avoid committing to a course of action? prematurely?
*)
let packet_version_id (xs : int list) : int * int * int list = 
   let (version , tl) = packet_version xs in
   let (ident , tl2) = packet_id tl in 
   (version , ident , tl2)


(* 
we can now get packet_version and packet_id from a list of int (our bools)

packet_version_id (hex2bools content);; 

let bools = hex2bools content in
  packet_version_id bools 

*)

(* if the ident is 4 then 

To do this, the binary number is padded with leading zeroes until its length is a multiple of four bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed by a 0 bit. These groups of five bits immediately follow the packet header. For example, the hexadecimal string D2FE28 becomes:

110100101111111000101000
VVVTTTAAAAABBBBBCCCCC

Below each bit is a label indicating its purpose:

    The three bits labeled V (110) are the packet version, 6.
    The three bits labeled T (100) are the packet type ID, 4, which means the packet is a literal value.
    The five bits labeled A (10111) start with a 1 (not the last group, keep reading) and contain the first four bits of the number, 0111.
    The five bits labeled B (11110) start with a 1 (not the last group, keep reading) and contain four more bits of the number, 1110.
    The five bits labeled C (00101) start with a 0 (last group, end of packet) and contain the last four bits of the number, 0101.
    The three unlabeled 0 bits at the end are extra due to the hexadecimal representation and should be ignored.

So, this packet represents a literal value with binary representation 011111100101, which is 2021 in decimal.
*)

exception BadDecodePV4 of int list

(* are we assuming well formed tails of packets with ident of 4 ?? 
how are we going to track the tail of the list ?
given a list of bools if the first item is a 1 then 

every packet 
V V V 
3 bits version 
      Id Id Id 
      3 bits id 

most significant bit encoding on binary values  100 boolean represents 4 in decimal 
reverse bits (msb first) 
 0 0 1
 1 2 4   so 1 x 4 = 4  decimal equivalent 

binary number is padded so its length is a multiple of 4 bits
then is broken into groups of 4 bits 

each group is prefixed by a 1 bit except last group which gets a 0 bit 

VVVIII1aaaa1bbbb0cccc
                ^---- last group 0 bit followed by 4 bits composing the number 
<-need to know how long this was---> 
this has length of 21 is that a multiple of 4 ??
5 * 4 = 20
6 * 4 = 24  

  then determine how many more bits we need to drop from the list since packet is a multiple of 4 bits?
if consumed exact multiple of 4 bits we drop 0 bits (no more consumed)
if consumed one bit of next character , we drop 3 bits
if consumed two bits of next char , we drop 2 bits
if consumed three bits of next char , we drop 1 bits
 
OR if we consume by hex character we are already consuming in 4 bit blocks , so we can ignore dropping
bits issue altogether

can we be lazily on the given input string ?

decode xs k  

*)
let rec decode_pv4 (xs : int list) : int list = 
 match xs with 
 | 1 :: b :: c :: d :: e :: tl -> [b;c;d;e] @ (decode_pv4 tl)
 | 0 :: b :: c :: d :: e :: tl -> [b;c;d;e]
 | _ -> raise (BadDecodePV4 xs)
 

(* packet version id of 4 *)
let pv4 (xs : int list) : (int list) option = 
   let (version , tl) = packet_version xs in
   let (ident , tl2) = packet_id tl in 
   if ident == 4 then Some (decode_pv4 tl2)
   else None

(* pv4 (hex2bools "D2FE28") 
we will ignore ...

[0; 1; 1; 1; 1; 1; 1; 0; 0; 1; 0; 1] -- ok 
011111100101
2021 in decimal

packet type id of 4 -- send to pv4
otherwise operator packet -- 

let packrat be our entry given we have a list of bools 
hex2bools
*)


let packrat (xs : int list) : (int list list) = 
  match pv4 xs with
    | Some res -> [res]  (* type id 4 --- int list*) 
    | None -> [] (* not type id 4 -- its an operator what do we do now ? *) 





(* trying to convert a list of bools [1;0;0;1;0;1; ...] into a decimal value 
but we hit a hurdle of int -- is it a fixed size fixnum if so it will overflow 
lets see how this works 

make foo take an initial number  n, 
p a rolling product 
result type of unit 

foo n p () 

*)

let rec factorial n =
  if n <= 1 then Z.one
  else Z.mul (Z.of_int n) (factorial (n - 1))

let rec foo2 (n : int) (p : Z.t) = 
  let p2 = Z.mul (Z.of_int n) p in 
  Printf.printf "Foo %d = %s\n" n (Z.to_string p2) ; 
  if n > 1000 then () else foo2 (n + 1) p2 

(* lets see what foo_demo does *)
let foo2_demo () = foo2 1 Z.one


(* 
let () =
  let n = 100 in
  let result = factorial n in
  Printf.printf "!%d = %s\n" n (Z.to_string result)
*)


let rec foo (n : int) (p : int) = 
  let p2 = n * p in 
  Printf.printf "Foo %d = %d\n" n p2 ; 
  if n > 1000 then () else foo (n + 1) p2 

(* lets see what foo_demo does *)
let foo_demo () = foo 1 1
(*
utop # Day16.Util.foo_demo ();;
Foo 1 = 1
Foo 2 = 2
Foo 3 = 6
Foo 4 = 24
Foo 5 = 120
Foo 6 = 720
Foo 7 = 5040
Foo 8 = 40320
Foo 9 = 362880
Foo 10 = 3628800
Foo 11 = 39916800
Foo 12 = 479001600
Foo 13 = 6227020800
Foo 14 = 87178291200
Foo 15 = 1307674368000
Foo 16 = 20922789888000
Foo 17 = 355687428096000
Foo 18 = 6402373705728000
Foo 19 = 121645100408832000
Foo 20 = 2432902008176640000
Foo 21 = -4249290049419214848
Foo 22 = -1250660718674968576
Foo 23 = -1095080418959949824
Foo 24 = 1388186055525531648
Foo 25 = -2188836759280812032
Foo 26 = -1569523520172457984
Foo 27 = 3739725139617513472
Foo 28 = 3255211503887843328
Foo 29 = 2167413244199698432
Foo 30 = 458793068007522304
Foo 31 = -4224158965476360192
Foo 32 = 3177493657578110976
Foo 33 = 3400198294675128320
Foo 34 = -4297094460157722624
Foo 35 = -2824353515843878912
Foo 36 = -219634164977106944
Foo 37 = 1096907932701818880
Foo 38 = -4434358741604761600
Foo 39 = 2304077777655037952
Foo 40 = -70609262346240000
Foo 41 = -2894979756195840000
Foo 42 = -1685313281113194496
Foo 43 = 1318505206970843136
Foo 44 = 2673996885588443136
Foo 45 = 426023372367855616
Foo 46 = 1150331055211806720
Foo 47 = -1274672626173739008
Foo 48 = 3379318201643958272
Foo 49 = -434104782832009216
Foo 50 = -3258495067890909184
Foo 51 = -162551799050403840
Foo 52 = 770678486233776128
Foo 53 = 3952471622971031552
Foo 54 = 1295910792775860224
Foo 55 = -2511882692165894144
Foo 56 = -2314850208468434944
Foo 57 = -2819253366733930496
Foo 58 = 2504001392817995776
Foo 59 = 162129586585337856
Foo 60 = 504403158265495552
Foo 61 = 3098476543630901248
Foo 62 = -1585267068834414592
Foo 63 = 1585267068834414592
Foo 64 = 0
Foo 65 = 0

err ... what just happended ... its convinced its zero after Foo 63 ?

overflowed somewhere somehow ..

*)


(*
(* may overflow int if bool list is too long *)
let rec bools_to_decimal2 (xs : int list) (pow : int) (acc : int) = 

let bools_to_decimal (xs : int list) : int = 
   let rv = List.reverse xs in 
   let acc = 0 in
   let pow = 1 in 
   bools_to_decimal2 rv pow acc
*)
