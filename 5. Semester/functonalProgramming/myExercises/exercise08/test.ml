type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);;
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec layer_tree i = LNode (i, (fun () -> layer_tree (i + 1)),(fun () -> layer_tree (i + 1)));;

let rec interval_tree (l,h) = LNode ((l,h), (fun () -> interval_tree (l,((l+h)/2))),(fun () -> interval_tree ((((l+h)/2)),h)));;

let rational_tree () = let rec rat_tree (n,d) = LNode ((n,d),(fun () -> rat_tree (n,(d+1))),(fun () -> rat_tree ((n+1),d))) in rat_tree (0,0);;

let rec top n (LNode (i,l,r)) = if n <= 0 then Empty else Node (i,(top (n-1) (l ())),(top (n-1) (r ())));;

let rec map f (LNode (i,l,r)) = LNode ((f i),(fun () -> map f (l ())),(fun () -> map f (r ())));;

let layer_list n t = let rec help n (LNode (i,l,r)) acc =
            if n = 0 then (LNode (i,l,r)::acc) else (help (n-1) (l()) [])@(help (n-1) (r()) [])@acc
            in help n t [];;

let rec find_help f tl = match tl with
            | [] -> None
            | (LNode (i,l,r))::xs -> if f i then Some (LNode (i,l,r)) else find_help f xs;;

let find f t = let rec help f t n = 
            match find_help f (layer_list n t) with 
            | None -> help f t (n+1)
            | Some (LNode (i,l,r)) -> LNode (i,l,r)
            in help f t 0;;

let x = layer_tree 2;;
top 3 x;;
layer_list 2 x;;

