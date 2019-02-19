let todo _ = failwith "TODO"

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
and state = var -> value option (* new *);;

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
    | Var i -> (match (s i) with 
                | None -> Fun (i,s,e)
                | Some j -> j )
    | Bind (x,e,b) -> eval_expr (fun z -> if z = x then Some (eval_expr s e) else s z) b
    | Ite (c,t,e) -> (match eval_expr s c with 
                | Rat (n1,d1) -> if n1 = 0 then eval_expr s e else eval_expr s t
                | _ -> failwith "invalid type"  )
    | Func (a,b) -> Fun (a,s,b)
    | App (f,a) -> (match eval_expr s f with 
                | Fun (v',s',e') -> (match eval_expr s a with 
                    | Rat (n1,d1) -> eval_expr (fun z -> if z = v' then Some (Rat (n1,d1)) else s' z) e' 
                    | Fun (v'',s'',e'') -> failwith "not yet implemented"  )
                | _ -> failwith "invalid type"  );;

let s = fun r -> if r = "_a" then Some (Rat (1,3)) else if r = "_b" then Some (Rat (1, 6)) else Some (Rat (0, 1));;

print_endline "Test 1";;
let x1 = BinOp (Add, Var "_a", Var "_b");;
eval_expr s x1;;
(Rat (1, 2));;

print_endline "Test 2";;
let x2 = Bind ("x", Const (2, 7), Var "x");;
eval_expr s x2;;
(Rat (2, 7));;

print_endline "Test 3";;
let x3 = Ite (Const (3, 4), Const (1, 2), Const (2, 1));;
eval_expr s x3;;
(Rat (1, 2));;

print_endline "Test 4";;
let x4 = Ite (BinOp (Mul, Const (1, 4), Const (0, 1)), Const (1, 2), Const (2, 1));;
eval_expr s x4;;
(Rat (2, 1));;

print_endline "Test 5";;
let x5 = Func ("z", BinOp (Sub, Const (1, 3), Var "z"));;
eval_expr s x5;;
(Fun ("z", (fun _ -> None), BinOp (Sub, Const (1, 3), Var "z")));;

print_endline "Test 6";;
let x6 = Bind ("f", Func ("x", BinOp (Mul, Var "x", Var "x")), App (Var "f", BinOp (Sub, Const (1, 2), Const (1, 4))));;
eval_expr s x6;;
(Rat (1, 16));;

print_endline "Test 7";;
let x7 = Bind ("add", Func ("x", Func ("y", BinOp (Add, Var "x", Var "y"))), Bind ("x", Const (2,3), App (App (Var "add", Var "x"), Const (4, 3))));;
eval_expr s x7;;
(Rat (2, 1));;

print_endline "Test 8";;
let x8 = App (Bind ("x", Const (2,1), Func ("a", Bind ("x", BinOp (Add, Var "a", Var "x"), BinOp (Mul, Var "x", Var "x")))), Const (10, 2));;
eval_expr s x8;;
(Rat (49, 1));;

(*
| Bind (x,e,b) -> (match eval_expr s e with 
                | Rat (n1,d1) ->
                    (match (eval_expr (fun z -> if z = x then Some (Rat (n1,d1)) else s z) b) with 
                    | Rat (n2,d2) -> Rat (n2,d2)
                    | _ -> failwith "ivalid type")
*)