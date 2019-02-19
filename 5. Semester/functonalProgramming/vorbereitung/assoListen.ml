
let find1 a l = match List.assoc_opt a l with
    | None -> (None,l)
    | Some x -> (Some x,((a,x)::List.remove_assoc a l ));;

type ('a,'b) mfu = (int * ('a * 'b)) list

let find2 a l = let l2 = List.map (fun (i,(a,b)) -> (a,(i,b))) l in
    let r = match List.assoc_opt a l2 with
        | None -> (None, l2)
        | Some (i,b) -> (Some b),(a,((i+1),b))::List.remove_assoc a l2 in
    let compare (a,(i,b)) (a',(i',b')) = if i > i' then 1 else if i = i' then 0 else -1 in
    match r with (a,l) ->
        a,(List.sort compare l) |> List.map (fun (a,(i,b)) -> (i,(a,b)));;