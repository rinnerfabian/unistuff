(* 1 *)
let is_empty m = m = [];;
let rec get k m = match m with
                    | [] -> None
                    | (a,b)::xs -> if k = a then Some b else get k xs;;
let put k v m = m @ [(k,v)];;
let contains_key k m = (get k m) <> None;;
let rec remove k m = match m with 
                    | [] -> []
                    | (a,b)::xs -> if k = a then remove k xs else (a,b)::remove k xs;;
let keys m = List.map fst m;;
let values m = List.map snd m;; 
(* 3 *)
let is_empty m = failwith "impossible";;
let rec get k m = m k;;
let put k v m = (fun x -> if x = k then Some v else m x);;
let contains_key k m = (get k m) <> None;;
let rec remove k m = (fun x -> if x = k then None else m x);;
let keys m = failwith "impossible";;
let values m = failwith "impossible";;
