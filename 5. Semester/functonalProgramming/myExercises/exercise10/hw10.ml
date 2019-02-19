
module type Ring = sig
  type t
  val zero : t
  val one : t
  val compare : t -> t -> int
  val to_string : t -> string
  val add : t -> t -> t
  val mul : t -> t -> t
end

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
end

(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)
(* Assignment 10.2 [20 Points] *)

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
      | x::xs,y::ys ->  if x = y then help xs ys (x::acc)
                        else if x > y then help (x::xs) ys acc else help xs (y::ys) acc
      | _,_ -> acc in help (List.sort F.compare a) (List.sort F.compare b) []
end;;

(* 6. functor DenseMatrix *)
module DenseMatrix (R : Ring) : Matrix with type elem = R.t = struct
    type elem = R.t
    type t = R.t list list
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

(* 7. functor SparseMatrix *)
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

(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let (|=) a b =
  List.sort compare a = List.sort compare b

let check_string_representation s elems =
  if String.length s < 2 then false else
  if String.get s 0 <> '{' then false else
  if String.get s (String.length s - 1) <> '}' then false else
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ','
  |> List.map String.trim
  |> (|=) elems

let tests =
  (****************************
   * tests for 10.2 (IntRing) :
   * NOTE: Comment tests until you have completed your implementation of IntRing
   *)
  
  let implementsRingSignature (module M : Ring) = true in
  [
  __LINE_OF__ (fun () -> implementsRingSignature (module IntRing));
  __LINE_OF__ (fun () -> IntRing.compare 9 10 < 0 && IntRing.compare 10 9 > 0 && IntRing.compare 10 10 = 0);
  __LINE_OF__ (fun () -> IntRing.add 10 IntRing.zero = 10);
  __LINE_OF__ (fun () -> IntRing.mul 10 IntRing.one = 10);
  __LINE_OF__ (fun () -> IntRing.to_string 10 = "10");
  ] @ 

  (******************************
   * tests for 10.2 (FloatRing) :
   * NOTE: Comment tests until you have completed your implementation of FloatRing
   *)
  
  let implementsRingSignature (module M : Ring) = true in
  [
  __LINE_OF__ (fun () -> implementsRingSignature (module FloatRing));
  __LINE_OF__ (fun () -> FloatRing.compare 9.5 10.0 < 0 && FloatRing.compare 10.0 9.5 > 0 && FloatRing.compare 10.0 10.0 = 0);
  __LINE_OF__ (fun () -> FloatRing.add 10.0 FloatRing.zero = 10.0);
  __LINE_OF__ (fun () -> FloatRing.mul 10.0 FloatRing.one = 10.0);
  __LINE_OF__ (fun () -> FloatRing.to_string 10.0 = "10.");
  ] @

  (*****************************
   * tests for 10.2 (BoolRing) :
   * NOTE: Comment tests until you have completed your implementation of BoolRing
   *)
  
  let implementsFiniteRingSignature (module M : FiniteRing) = implementsRingSignature (module M) in
  [
  __LINE_OF__ (fun () -> implementsFiniteRingSignature (module BoolRing));
  __LINE_OF__ (fun () -> BoolRing.compare BoolRing.zero BoolRing.one < 0 && BoolRing.compare BoolRing.one BoolRing.zero > 0 && BoolRing.compare BoolRing.zero BoolRing.zero = 0);
  __LINE_OF__ (fun () -> BoolRing.add true BoolRing.zero = true && BoolRing.add false BoolRing.zero = false);
  __LINE_OF__ (fun () -> BoolRing.mul true BoolRing.one = true && BoolRing.mul false BoolRing.one = false);
  __LINE_OF__ (fun () -> BoolRing.to_string true = "true");
  __LINE_OF__ (fun () -> BoolRing.elems |= [true;false]);
  ] @ 

  (****************************
   * tests for 10.2 (SetRing) :
   * NOTE: Comment tests until you have completed your implementation of SetRing
   *)
  
  let module TestRing : FiniteRing with type t = char = struct
    let cfrom x = (int_of_char x) - (int_of_char 'a')
    let cto x = char_of_int (x mod 4 + int_of_char 'a')

    type t = char
    let zero = 'a'
    let one = 'd'
    let compare = Pervasives.compare
    let to_string c = Printf.sprintf "'%c'" c
    let add a b = (cfrom a) + (cfrom b) |> cto
    let mul a b = (cfrom a) * (cfrom b) |> cto
    let elems = ['a'; 'b'; 'c'; 'd']
  end in
  let module SR = SetRing (TestRing) in
  [
  __LINE_OF__ (fun () -> SR.zero = [] && SR.one |= ['a'; 'b'; 'c'; 'd']);
  __LINE_OF__ (fun () -> SR.compare ['b';'d'] ['a'] > 0);
  __LINE_OF__ (fun () -> SR.compare ['c';'b'] ['c';'d'] < 0);
  __LINE_OF__ (fun () -> SR.compare ['a';'d'] ['d';'a'] = 0);
  __LINE_OF__ (fun () -> SR.add ['a';'b'] ['c';'b'] |= ['a';'b';'c']);
  __LINE_OF__ (fun () -> SR.add ['b';'d'] SR.zero |= ['b';'d']);
  __LINE_OF__ (fun () -> SR.mul ['a';'b'] ['c';'b'] |= ['b']);
  __LINE_OF__ (fun () -> SR.mul ['a';'b'] SR.one |= ['a';'b']);
  __LINE_OF__ (fun () -> check_string_representation (SR.to_string SR.one) ["'a'";"'b'";"'c'";"'d'"]);
  ] @

  (********************************
   * tests for 10.2 (DenseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of DenseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
   *)
  
  let module DM = DenseMatrix (IntRing) in
  let dm0 = DM.from_rows [[4;-2;1];[0;3;-1]] in
  let dm1 = DM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_dense m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = DM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_dense (DM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_dense (DM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.set 1 0 7 (DM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.transpose dm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_dense (DM.add dm0 dm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_dense (DM.mul dm0 dm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (DM.to_string dm0) = "4 -2 1\n0 3 -1");
  ] @ 

  (*********************************
   * tests for 10.2 (SparseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of SparseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
   *)
  
  let module SM = SparseMatrix (IntRing) in
  let sm0 = SM.from_rows [[4;-2;1];[0;3;-1]] in
  let sm1 = SM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_sparse m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = SM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_sparse (SM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_sparse (SM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.set 1 0 7 (SM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.transpose sm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.add sm0 sm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_sparse (SM.mul sm0 sm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (SM.to_string sm0) = "4 -2 1\n0 3 -1");
  ] @
  []


let () =
  let rec input_lines ch =
    (try Some (input_line ch) with _ -> None) (* catch stupid EOF exception *)
    |> function Some line -> line :: input_lines ch | None -> []
  in
  let lines = input_lines (open_in __FILE__) in
  let open List in
  let open Printf in
  let fail l =
    let line = nth lines (l-1) in
    let test = String.sub line 25 (String.length line - 27) in
    printf "test \027[31;m%s\027[0;m (line %d) failed!\n" test l;
  in
  let test (l, t) =
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)


