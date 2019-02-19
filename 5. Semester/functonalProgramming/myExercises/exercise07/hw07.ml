let todo _ = failwith "TODO"

(* 7.5 type definitions *)
type rat = int * int (* num, denom *)
type var = string (* new *)
type binary_op = Add | Sub | Mul | Div
type unary_op = Neg
type expr = Const of rat
          | UnOp of unary_op * expr
          | BinOp of binary_op * expr * expr
          | Var of var (* new *)
          | Func of var * expr (* new *)
          | Bind of var * expr * expr (* new *)
          | App of expr * expr (* new *)
          | Ite of expr * expr * expr (* new *)
type value = Rat of rat | Fun of var * state * expr (* new *)
and state = var -> value option (* new *)


(* 7.6 type definitions *)
type graph = (int * float * int) list


(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)
(* Assignment 7.4 [7 points] *)
let f1 a b = a+1;;
let f2 a b = if List.length b > List.length a then b else a;;
let f3 a b = match b with (x,y) -> a @ [(y,x)];;
let f4 a x = List.rev(a @ [x]);;
let f5 a x = match x with (l,m) -> fun z -> if z = l then m else a z;;
let f6 a x = match a with [] -> failwith "unreachable" | y::ys -> [(x y)] @ a;;
let f7 a x = (a*a)*x;;

(*****************************************************************************)
(* Assignment 7.5 [7 points] *)
let rec eval_expr (s : state) (e : expr) : value =
    match e with
    | Const c -> Rat c
    | UnOp (Neg, e) -> (match eval_expr s e with
        | Rat (n, d) -> Rat (-n, d)
        | _ -> failwith "invalid type") 
    | BinOp (op, e1, e2) ->
        (match eval_expr s e1, eval_expr s e2 with
        | Rat (n1, d1), Rat (n2, d2) ->
        (match op with
        | Add -> Rat (n1*d2+n2*d1,d1*d2)
        | Sub -> Rat (n1*d2-n2*d1,d1*d2)
        | Mul -> Rat (n1*n2,d1*d2)
        | Div -> Rat (n1*d2,d1*n2))
        | _ -> failwith "invalid type")
    (* TODO: continue here *)
    | Var i -> (match (s i) with None -> Fun (i,s,e) | Some j -> j)
    | Func (a,b) -> Fun (a,s,b)
    | Bind (x,e,b) -> eval_expr (fun z -> if z = x then Some (eval_expr s e) else s z) b
    | Ite (c,t,e) -> (match eval_expr s c with 
                | Rat (n1,d1) -> if n1 = 0 then eval_expr s e else eval_expr s t
                | _ -> failwith "invalid type") (* not sure: invalid type or same as not 0 *)
    | App (f,a) -> (match eval_expr s f with 
                | Fun (v',s',e') -> (match eval_expr s a with 
                    | Rat (n1,d1) -> eval_expr (fun z -> if z = v' then Some (Rat (n1,d1)) else s' z) e' 
                    | _ -> failwith "invalid type")
                | _ -> failwith "invalid type");;

(*****************************************************************************)
(* assignment 7.6 [6 points] *)
(* Hilfsfunktionen *)
(* Graph nach Kantengewichte sortieren *)
let my_compare a b = match a,b with 
                | (n,e,m),(n',e',m') -> if e > e' then 1 else if e = e' then 0 else -1;;
let sort l = List.sort my_compare l;;
(* Liste von Nachbarn eines Knoten in einem Graphen ausgeben *)
let rec neighbors i l e = match l with 
                    | [] -> []
                    | (n1,y,n2)::xs ->  if n1 = i && n2 <> e then n2::(neighbors i xs e)
                                        else    if n2 = i && n1 <> e then n1::(neighbors i xs e)
                                                else (neighbors i xs e);;

let rec f s e nl ex = match s with 
        | [] -> true
        | x::xs -> if x = e then false else (f xs e nl ex) && (f (neighbors x nl ex) e nl x);;

let mst l = let rec help sl a = match sl with
        | [] -> a
        | (n1,y,n2)::xs ->    if a = [] then help xs ((n1,y,n2)::a)
                            else
                                if (f [n1] n2 a (-1)) then help xs ((n1,y,n2)::a)
                                else help xs a
        in help (sort l) [];;


(*****************************************************************************)
(***************************** HOMEWORK ENDS HERE ****************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
(*
  _a + _b
*)
let a75_ex1 = BinOp (Add, Var "_a", Var "_b")

(*
  let x = 2/7 in x
*)
let a75_ex2 = Bind ("x", Const (2, 7), Var "x")

(*
  if 3/4 then 1/2 else 2/1
*)
let a75_ex3 = Ite (Const (3, 4), Const (1, 2), Const (2, 1))

(*
  if 1/4 * 0/1 then 1/2 else 2/1
*)
let a75_ex4 = Ite (BinOp (Mul, Const (1, 4), Const (0, 1)), Const (1, 2), Const (2, 1))

(*
  (fun z -> 1/3 - z)
*)
let a75_ex5 = Func ("z", BinOp (Sub, Const (1, 3), Var "z"))

(*
  let f = fun x -> x * x in
  f (1/2 - 1/4)
*)
let a75_ex6 = Bind ("f", Func ("x", BinOp (Mul, Var "x", Var "x")), App (Var "f", BinOp (Sub, Const (1, 2), Const (1, 4))))

(*
  let add = fun x -> fun y -> x + y in
  let x = 2/3 in
  add x 4/3
*)
let a75_ex7 = Bind ("add", Func ("x", Func ("y", BinOp (Add, Var "x", Var "y"))), Bind ("x", Const (2,3), App (App (Var "add", Var "x"), Const (4, 3))))

(*
  (let x = 2/1 in fun a -> let x = a + x in x * x) 10/2
*)
let a75_ex8 = App (Bind ("x", Const (2,1), Func ("a", Bind ("x", BinOp (Add, Var "a", Var "x"), BinOp (Mul, Var "x", Var "x")))), Const (10, 2))


let a76_ex1 = [0,1.,1; 0,4.,2; 1,2.,2; 1,1.,3; 2,3.,3]
let a76_ex2 = [0,4.,1; 0,4.,7; 1,8.,2; 1,11.,7; 2,7.,3; 2,4.,5; 2,2.,8; 3,9.,4; 3,14.,5; 4,10.,5; 5,2.,6; 6,1.,7; 6,6.,8; 7,7.,8;]

(*****************************************************************************)
(* TESTS [do not change] *)
let (=.) a b = (abs_float (a -. b)) < 0.001
let print_mst_for t =
  print_endline ("[" ^ (String.concat "; " (List.map (fun (s,w,d) -> "(" ^ (string_of_int s) ^ "," ^ (string_of_float w) ^ "," ^ (string_of_int d) ^ ")") t)) ^ "]")

let simp (n,d) =
  let k, n = if n < 0 then -1, -n else 1, n in
  let k, d = if d < 0 then -k, -d else k, d in
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  let g = gcd n d in
  (k * n / g, d / g)

let test_ee p e =
  let s = fun r -> if r = "_a" then Some (Rat (1,3)) else if r = "_b" then Some (Rat (1, 6)) else Some (Rat (0, 1)) in
  match e, eval_expr s p with
  | Rat (en, ed), Rat (pn, pd) -> (en,ed) = simp (pn, pd)
  | Fun (ea, _, ed), Fun (pa, _, pd) -> ea = pa && ed = pd
  | _, _ -> false

let test_mst g e =
  let sort_t l = List.map (fun (s,w,d) -> if d < s then (d, w, s) else (s, w, d)) l |>
    List.sort (fun (s1,_,d1) (s2,_,d2) -> if s1 = s2 then compare d1 d2 else compare s1 s2) in
  let t1 = (sort_t (mst g)) in
  let t2 = (sort_t e) in
  let rec cmp l1 l2 = match l1, l2 with [],[] -> true
    | (s1,w1,d1)::xs, (s2,w2,d2)::ys -> s1 = s2 && d1 = d2 && w1 =. w2 && cmp xs ys
    | _, _ -> false
  in
(* print_mst_for t1; print_mst_for t2; *)
  cmp t1 t2

let tests = [
  (* tests for 7.4 *)
  __LINE_OF__ (fun () -> (List.fold_left f1 0 ["ab"; "xx"; "ab"; "u"; "iuw"; "bb"]) = 6);
  __LINE_OF__ (fun () -> (List.fold_left f2 [] [[1;2;3]; [0]; [7;9;1;3]; []; [1]]) = [7;9;1;3]);
  __LINE_OF__ (fun () -> (List.fold_left f3 [] [(1,2); (3,4); (5,6)]) = [(2,1); (4,3); (6,5)]);
  __LINE_OF__ (fun () -> (List.fold_left f4 [] ['a';'b';'c';'d';'e';'f';'g']) = ['g';'e';'c';'a';'b';'d';'f']);
  __LINE_OF__ (fun () -> let g = List.fold_left f5 (fun _ -> 0) [('a',3); ('z', -9); ('d', 18)] in (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9));
  __LINE_OF__ (fun () -> (List.fold_left f6 [0] [(fun x -> x + 3); (fun x -> x * x); (fun x -> x * -4)]) = [-36;9;3;0]);
  __LINE_OF__ (fun () -> (List.fold_left f7 (-3) [6;-2;3]) = 102036672);
  (* tests for 7.5 *)
  __LINE_OF__ (fun () -> test_ee a75_ex1 (Rat (1, 2)));
  __LINE_OF__ (fun () -> test_ee a75_ex2 (Rat (2, 7)));
  __LINE_OF__ (fun () -> test_ee a75_ex3 (Rat (1, 2)));
  __LINE_OF__ (fun () -> test_ee a75_ex4 (Rat (2, 1)));
  __LINE_OF__ (fun () -> test_ee a75_ex5 (Fun ("z", (fun _ -> None), BinOp (Sub, Const (1, 3), Var "z"))));
  __LINE_OF__ (fun () -> test_ee a75_ex6 (Rat (1, 16)));
  __LINE_OF__ (fun () -> test_ee a75_ex7 (Rat (2, 1)));
  __LINE_OF__ (fun () -> test_ee a75_ex8 (Rat (49, 1)));
  (* tests for 7.6 *)
  __LINE_OF__ (fun () -> test_mst a76_ex1 [0,1.,1; 1,2.,2; 1,1.,3]);
  __LINE_OF__ (fun () -> test_mst a76_ex2 [0,4.,1; 0,4.,7; 2,7.,3; 2,4.,5; 2,2.,8; 3,9.,4; 5,2.,6; 6,1.,7]);
]

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

