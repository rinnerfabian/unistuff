
let hd l = match l with 
            | [] -> failwith "invalid"
            | x::xs -> x;;

let tl l = match l with 
            | [] -> []
            | x::xs -> xs;;

let rec length l = match l with 
            | [] -> 0
            | x::xs -> 1 + length xs;;

let rec append l1 l2 = match l1,l2 with
            | [],_ -> l2
            | _,[] -> l1
            | x::xs,_ -> x:: append xs l2;;

let rec rev l = match l with 
            | [] -> []
            | x::xs -> append (rev xs) [x];;

let rec nth n l = if n < 0 then failwith "invalid"
                else if n = 0 then hd l
                else match l with 
                        | [] -> failwith "invalid"
                        | x::xs -> nth (n-1) xs;;
