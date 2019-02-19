type graph = (int * float * int) list;;
(* Hilfsfunktionen *)
(* Graph nach Kantengewichte sortieren *)
let compare a b = match a,b with 
                | (n,e,m),(n',e',m') -> if e > e' then 1 else if e = e' then 0 else -1;;
let sort l = List.sort compare l;;
(* Sucht false in einer Liste wenn findet dann false sonst true *)
let rec search_f l = match l with 
        | [] -> true
        | x::xs -> if x = false then false else search_f xs;;
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

let x = [0,1.,1; 0,4.,2; 1,2.,2; 1,1.,3; 2,3.,3];;
mst x;;
let y = [0,4.,1; 0,4.,7; 1,8.,2; 1,11.,7; 2,7.,3; 2,4.,5; 2,2.,8; 3,9.,4; 3,14.,5; 4,10.,5; 5,2.,6; 6,1.,7; 6,6.,8; 7,7.,8;];;
print_endline "hieeeer";;
mst y;;
[0,4.,1; 0,4.,7; 2,7.,3; 2,4.,5; 2,2.,8; 3,9.,4; 5,2.,6; 6,1.,7];;