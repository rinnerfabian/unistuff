
module type Ring = sig
  type t
  val zero : t
  val one : t
  val compare : t -> t -> int
  val to_string : t -> string
  val add : t -> t -> t
  val mul : t -> t -> t
end;;

module type Matrix = sig
  type elem
  type t
  val create : int -> int -> t
  val identity : int -> t
  val from_rows : elem list list -> t
  val to_string : t -> string
  val set : int -> int -> elem -> t -> t
  val get : int -> int -> t -> elem
  val transpose : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end;;

(* 1. module IntRing *)
module IntRing : Ring with type t = int = struct
    type t = int
    let zero = 0
    let one = 1
    let compare a b = if a > b then 1 else if a < b then (-1) else 0
    let to_string t = string_of_int t
    let add a b = a + b
    let mul a b = a * b
end;;

(* 2. module FloatRing *)
module FloatRing : Ring with type t = float = struct
    type t = float
    let zero = 0.0
    let one = 1.0
    let compare a b = if a > b then 1 else if a < b then (-1) else 0
    let to_string t = string_of_float t
    let add a b = a +. b
    let mul a b = a *. b
end;;

(* 3. signature FiniteRing *)
module type FiniteRing = sig
  include Ring
  val elems : t list
end;;

(* 4. module BoolRing *)
module BoolRing : FiniteRing with type t = bool = struct
    type t = bool
    let elems = [true;false]
    let zero = false
    let one = true
    let compare a b = if a > b then 1 else if a < b then (-1) else 0
    let to_string t = string_of_bool t
    let add a b = if not a && not b then false else true
    let mul a b = if a && b then true else false
end;;

(* 5. functor SetRings *)
module SetRing (F : FiniteRing) : Ring with type t = F.t list = struct
    type t = F.t list
    let zero = []
    let one = F.elems
    let compare a b = match (List.sort F.compare a),(List.sort F.compare b) with a',b' -> 
        if a' > b' then 1 else if a' < b' then (-1) else 0
    let rec to_string s = let rec help s = match s with
      | [] -> ""
      | x::xs -> if xs = [] then (F.to_string x)
                            else (F.to_string x) ^ "," ^ help xs
      in "{" ^ (help s) ^ "}"
    let add a b = let rec help a b acc = match a,b with
      | [],[] -> acc
      | x::xs,[] -> acc@(x::xs)
      | [],y::ys -> acc@(y::ys)
      | x::xs,y::ys -> if x = y then help xs ys (x::acc) else help xs (y::ys) (x::acc)
      in help (List.sort F.compare a) (List.sort F.compare b) []
    let mul a b = let rec help a b acc = match a,b with
      | x::xs,y::ys ->  if x = y
                        then help xs ys (x::acc)
                        else
                          if x > y then help (x::xs) ys acc
                          else help xs (y::ys) acc
      | _,_ -> acc
      in help (List.sort F.compare a) (List.sort F.compare b) []
end;;

(* 6. functor DenseMatrix *)
module DenseMatrix (R : Ring) : Matrix with type elem = R.t = struct
    type elem = R.t
    type t = elem list list
    let create n m = let rec help m = if m = 0 then [] else R.zero::help (m-1) in 
                     let rec help2 n acc = if n = 0 then acc else help2 (n-1) ((help m)::acc)
                     in (help2 n [])
    let identity n = let rec help n i = if n = 0 then [] else
                        if n = i then ((help (n-1) i)@[R.one]) else ((help (n-1) i)@[R.zero]) in 
                     let rec help2 n m acc = if m = 0 then acc else help2 n (m-1) ((help n m)::acc)
                     in (help2 n n [])
    let from_rows l = l
    let to_string t = let rec help = function
      | [] -> ""
      | x::xs -> if xs = [] then (R.to_string x) ^ (help xs) else (R.to_string x)^ " " ^ (help xs) in
      let rec help2 = function
        | [] -> ""
        | y::ys -> if ys = []then (help y) ^ (help2 ys) else (help y) ^ "\n" ^ (help2 ys)
      in help2 t
    let set i j elem t = let rec help j elem = function
      | [] -> failwith "invalid column"
      | x::xs -> if j = 0 then elem::xs else x::(help (j-1) elem xs) in 
      let rec help2 i j elem = function
        | [] -> failwith "invalid row"
        | y::ys -> if i = 0 then (help j elem y)::ys else y::(help2 (i-1) j elem ys)
      in help2 i j elem t
    let get i j t = let rec help2 j t = match t with
        | [] -> failwith "invalid column"
        | x::xs -> if j = 0 then x else help2 (j-1) xs in
        let rec help i j t = match t with
        | [] -> failwith "invalid row"
        | x::xs -> if i = 0 then help2 j x else help (i-1) j xs
        in help i j t
    let transpose t = let init = let rec initial i acc = if i = 0 then acc else initial (i-1) ([]::acc) in
        (initial (match t with | [] -> failwith "unreachable" | x::xs -> List.length x) []) in
        let rec insert e i l = match l with
        | [] -> l
        | x::xs -> if i = 0 then (x@[e])::xs else x::(insert e (i-1) xs) in 
        let rec help l i acc2 = match l with 
          | [] -> acc2
          | y::ys -> help ys (i+1) (insert y i acc2) in
        let rec help2 t i acc = match t with 
          | [] -> acc
          | z::zs -> help2 zs (i+1) (help z 0 acc)
        in help2 t 0 init
    let add a b = let rec help l1 l2 = match l1,l2 with
        | [],[] -> []
        | x::xs,y::ys -> (R.add x y)::help xs ys
        | _,_ -> failwith "invalid input" in
        let rec help2 a b = match a,b with
        | [],[] -> []
        | x::xs,y::ys -> (help x y)::help2 xs ys 
        |_,_ -> failwith "invalid input" in help2 a b
    let mul a b = let rec help l1 l2 = match l1,l2 with
        | [],[] -> R.zero
        | x::xs,y::ys -> R.add (R.mul x y) (help xs ys)
        | _,_ -> failwith "invalid input" in
        let rec help2 a b = match a,b with
        | _,[] -> []
        | f::fs,g::gs -> (help f g)::(help2 a gs)
        | _,_ -> failwith "invalid input" in
        let rec help3 a b = match a,b with
        | [],_ -> []
        | h::hs,j::js -> help2 a b::help3 hs b
        | _,_ -> failwith "invalid input"
        in help3 a (transpose b)
end;;

module SparseMatrix (R : Ring) : Matrix with type elem = R.t = struct
  type elem = R.t
  type t = Mat of int * int * (R.t list list) * int list
  let create n m = let nm = let rec help n m = if n = 0 then [] else
      m::help (n-1) m
      in help n m in
      Mat (n,m,[],nm)
  let identity n = Mat (0,0,[],[])
  let from_rows l = Mat (0,0,[],[])
  let to_string t = ""
  let set n m elem t = Mat (0,0,[],[])
  let get n m t = R.one
  let transpose t = Mat (0,0,[],[])
  let add t1 t2 = Mat (0,0,[],[])
  let mul t1 t2 = Mat (0,0,[],[])
end;;

let module SM = SparseMatrix (IntRing) in
SM.create;;
