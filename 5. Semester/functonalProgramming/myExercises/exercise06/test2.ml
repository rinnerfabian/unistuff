
type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

let left t = match t with 
            | Empty -> Empty
            | Node (a,l,r) -> l;;

let right t = match t with 
            | Empty -> Empty
            | Node (a,l,r) -> r;;

let first_item l = match l with
                | [] -> Empty
                | x::xs -> x;;

let pop_item l = match l with 
                | [] -> []
                | x::xs -> xs;;

let rec sum_up l = match l with
                | [] -> 0
                | x::xs -> if x = Up then 1 + sum_up xs else sum_up xs;;

let search_parts l i = if i > sum_up l then [] else let rec search_parts_help l i j = 
                       match l with 
                       | [] -> []
                       | x::xs -> match x,(i<j) with 
                                | Up,false -> search_parts_help xs i (j+1)
                                | Up,true -> []
                                | _,true -> x::search_parts_help xs i j
                                | _,_ -> search_parts_help xs i j
                        in search_parts_help l i 1;;  

let crawl cl t = let rec crawl_help cl t s =
                match t with 
                | Empty -> (match cl with 
                        | [] -> Empty
                        | x::xs -> (match x with 
                                | New i -> crawl_help xs (Node (i, Empty,Empty)) s
                                | _ -> crawl_help xs Empty s))
                | Node (i,l,r) -> match cl with 
                        | [] -> Node (i,l,r)
                        | x::xs -> match x with 
                                | Left -> Node (i,crawl_help xs (left t) s,r)
                                | Right -> Node (i,l,crawl_help xs (right t) s)
                                | New i -> crawl_help xs (Node (i,Empty,Empty)) s
                                | Delete -> crawl_help xs Empty s
                                | Push -> crawl_help xs t (t::s)
                                | Pop -> crawl_help xs (first_item s) (pop_item s)
                                | _ -> failwith "unreachable"
                in crawl_help cl t [];;
 
let a68_t_l = Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty));;
let a68_t_r = Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty));;
let a68_t = Node (4, a68_t_l , a68_t_r);;

print_endline "hiiiiier";;

crawl [New 3] Empty
let x = Node (3, Empty, Empty);;
crawl [New 3] a68_t;;
let x = Node (3, Empty, Empty);;
crawl [New 3; Right; New 2] Empty;;
let x = Node (3, Empty, Node (2, Empty, Empty));;
crawl [Left; New 3] a68_t;;
let x = Node (4, Node (3, Empty, Empty), a68_t_r);;
crawl [Right; New 3] a68_t;;
let x = Node (4, a68_t_l, Node (3, Empty, Empty));;
crawl [Left; Delete] a68_t;;
let x = Node (4, Empty, a68_t_r);;
crawl [Left; Delete; New 8] a68_t;;
let x = Node (4, Node (8, Empty, Empty), a68_t_r);;
crawl [Left; Push; Right; Pop] a68_t;;
let x = Node (4, Node (2, Node (1, Empty, Empty), Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))), a68_t_r);;
