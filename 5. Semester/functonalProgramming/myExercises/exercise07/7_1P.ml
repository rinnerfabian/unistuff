let x = [4;1;2;3;6];;

let squaresum l = List.fold_left (fun a x -> a + x*x) 0 l;;
let float_list l = List.map float_of_int l;;
let to_string l = (List.fold_left (fun a x -> a ^ (string_of_int x) ^ ";") "[" l) ^ "]";;
let part_even l = List.fold_left (fun a x -> if x mod 2 = 0 then [x] @ a else a @ [x]) [] l;;
let part_even2 l = List.filter (fun x -> x mod 2 = 0) l @ List.filter (fun x -> x mod 2 <> 0) l;;
let part_even3 l = match List.partition (fun x -> x mod 2 = 0) l with (a,b) -> a @ b;;

squaresum x;;
float_list x;;
to_string x;;
part_even x;;
part_even2 x;;
part_even3 x;;