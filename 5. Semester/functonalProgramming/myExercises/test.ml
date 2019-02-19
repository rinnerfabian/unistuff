(*
let find1 a l = match (List.assoc_opt a l) with
    | None -> None,l
    | Some x -> Some x,((a,x)::List.remove_assoc a l);;

type ('a,'b) mfu = (int * ('a * 'b)) list;;

let x = [("baz", 5); ("foo", -3); ("bar", 4)];;

let init l = List.map (fun x -> (0,x)) l;;

let find2 a l = 

open Thread
open Event
type ('a,'b) t = Publish of 'a * 'b channel
            | Request of 'a * 'b event option channel


let broker () = 
    let c = new_channel () in
    let rec server_fun pList = 
        match (sync (receive c)) with 
            | Publish (a,answer_c) -> server_fun ((a,answer_c)::pList)
            | Request (a,answer_c) ->
                
                sync (send answer_c (List.assoc_opt a pList));
                server_fun pList
    in let _ = create server_fun [] in 
    c

let publish t a b =
    let c = new_channel () in
    sync (send t (a,c));
    let task () = 
        match (sync (receive c)) with
            | *)

((fun x -> x 3) (fun y z -> z y)) (fun w -> w + w)  ;;








