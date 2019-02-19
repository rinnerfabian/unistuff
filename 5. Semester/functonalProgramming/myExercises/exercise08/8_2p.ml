(*
8_1
let b : 'a -> int -> int -> int = (fun a b -> (+) b);;
let c : ('a -> string) -> (string -> 'a) -> string list = (fun a b c -> b (c a) :: [a]) "x";;
let d : int list -> (int -> (int -> int) -> int) -> int = (fun a b -> List.fold_left b 1 (List.map ( * ) a));;
let e : 'a list -> ('a -> bool) list = (let x = List.map in x (<));; *)

(* 8_2 2) *)
let fac n = let rec help n acc = if n < 2 then acc else help (n-1) (acc*n) in help n 1;;

let remove a l = let rec help a l acc = match l with 
        | [] -> acc
        | x::xs -> if x = a then help a xs acc else help a xs (x::acc)
        in List.rev (help a l []);;
