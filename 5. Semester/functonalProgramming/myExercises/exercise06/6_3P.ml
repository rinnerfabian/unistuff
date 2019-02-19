
(* Binäre Suchbäume *)

type tree = Leaf
            | Node of int * tree * tree;;

let t1 = Node (9,Node(6,Node(1,Leaf,Leaf),Node(8,Leaf,Leaf)),Node (42,Node (12,Leaf,Leaf),Leaf));;

let rec insert i t = match t with 
                | Leaf -> Node (i,Leaf,Leaf)
                | Node (a,l,r) -> if i = a
                                    then Node (a,l,r)
                                    else
                                        if i < a
                                        then Node (a,insert i l,r)
                                        else Node (a,l,insert i r);;

let t2 = insert 7 (Node (6, Leaf,Node (8,Leaf,Leaf)));;

let rec to_list t = match t with
                | Leaf -> []
                | Node (a,l,r) -> to_list l @ [a] @ to_list r;;

to_list t1;;

let rec remove_max t = match t with 
                | Leaf -> failwith "unreachable"
                | Node (a,l,Leaf) -> a,l
                | Node (a,l,r) -> let a',r' = remove_max r in a',Node (a,l,r')
in
let rec remove i t = match t with 
                | Leaf -> Leaf
                | Node (a,l,r) -> if i < a then Node (a,remove i l,r)
                                else if i > a then Node (a,l, remove i r)
                                else if l = Leaf then r
                                else let a',l' = remove_max l in Node (a',l',r)
in
remove 8 t1;;