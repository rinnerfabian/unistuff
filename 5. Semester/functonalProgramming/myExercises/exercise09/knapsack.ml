type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

let my_compare (t,i,w) (t',i',w') = if w > w' then 1 else if w < w' then -1 else 0;;
let sort_by_weight l = List.sort my_compare l;;
let initial i w = let list = let rec help i acc = if i = 0 then acc else help (i-1) ([]::acc)
        in help i []
        in let start = let rec help2 j = if j = (-1) then [] else j::help2 (j-1)
        in help2 w
        in List.rev(start)::list;;
let rec get2 j l = match l with
        | [] -> failwith "Spalte gibt es nicht!"
        | x::xs -> if j = 0 then x else get2 (j-1) xs;;
let rec get i j l = match l with
        | [] -> failwith "Zeile gibt es nicht!"
        | x::xs -> if i = 0 then get2 j x else get (i-1) j xs;;
let rec insert e i l = match l with
        | [] -> l
        | x::xs -> if i = 0 then (x@[e])::xs else x::(insert e (i-1) xs) ;;
let max a b = if a > b then a else b;;

let x = ["a",5,4; "b",2,2; "b",2,2; "d",4,5; "b",2,2; "e",8,2];;
let y = ["a",5,4; "a",5,4; "c",11,6; "d",4,5; "e",8,2; "a",5,4];;
let z = [("0",24,7);("1",50,6);("2",37,6);("3",19,1);("4",32,2);("5",71,8);("6",54,2);("7",87,8);("8",9,6);("9",80,9);("10",21,9);("11",93,0);("12",5,4);("13",97,5);("14",34,8);("15",42,7);("16",11,1);("17",71,4);("18",1,8);("19",39,0)];;
let z2 = [("0",16,6);("1",65,4);("2",26,0);("3",67,4);("4",93,9);("5",85,0);("6",2,6);("7",6,1);("8",76,4);("9",30,1);("10",50,1);("11",50,4);("12",53,5);("13",19,9);("14",1,3)];;

let matrix lt w =
    let rec help l w acc i j = match l with 
        | [] -> acc
        | (t,im,we)::xs -> if j > w then help xs w acc (i+1) 0 else 
                if we <= (get 0 j acc)
                then if i = 1
                        then help l w (insert im i acc) i (j+1)
                        else help l w (insert (max (im+(get (i-1)(j-we) acc)) (get (i-1) j acc)) i acc) i (j+1)
                else if i = 1
                        then help l w (insert 0 i acc) i (j+1)
                        else help l w (insert (get (i-1) j acc) i acc) i (j+1)
    in help (sort_by_weight lt) w (initial (List.length lt) w) 1 0;;

let knapsack lt w =
        let rec help l ma i j acc = match l with 
        | [] -> acc
        | (t,im,we)::xs -> match (get i j ma) with z -> if z = 0 then acc
        else if i = 1 then t::acc
        else 
                if z = (get (i-1) j ma)
                then help xs ma (i-1) j acc
                else help xs ma (i-1) (j-we) (t::acc)
        in help (List.rev(sort_by_weight lt)) (matrix lt w) (List.length lt) w [];;

knapsack z2 20;;
["2";"5";"7";"9";"10";"1";"3";"8";"12"];;

knapsack z 20;;
["11";"19";"3";"6";"17";"13";"7"];;