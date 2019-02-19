let x = [(fun x -> x+3);(fun x -> x*x)];;

let f1 a x = a+1;;

let f2 a x = if List.length x > List.length a then
                x
                else a;;

let f3 a x = match x with (x,y) -> a @ [(y,x)];;

let f4 a x = List.rev(a @ [x]);;

let f5 a x = match x with (l,m) -> fun z -> if z = l then m else a z;;

let f6 a x = match a with 
            | [] -> failwith "unreachable"
            | y::ys -> [(x y)] @ a;;

let rec pow x a = if a = 0 then 1 else x * pow x (a-1);;
let f7 a x = (a * a)*x;;

List.fold_left f7 (-3) [6;-2;3];;
102036672;;
