module List = struct 
    include List
    let init n f =
    let rec aux x acc = if x >= 0 then aux (x-1) (f x::acc) else acc
    in aux (n-1) []
end

open Thread
open Event

let help c =
    let count i = 
    let s = sync (receive c) in
    match s with 
        | None -> failwith "unreachable"
        | Some b ->
            let t = Printf.sprintf "Thread %2d: %d" (id (self ())) b in
            (print_endline t;
            (sync (send c None)))
  in
  create count 0;;

let asdf m = 
    let channels = List.init (List.length m) (fun _ -> new_channel ()) in
    let counters = List.map (help) channels in
    let rec run l ch = match l,ch with
        | [],[] -> ()
        | x::xs,c::cs -> sync (send c (Some x));
            if sync (receive c) = None then run xs cs else failwith "unreachable"
        | _,_ -> failwith "unreachable"
  in
  run m channels;
  List.iter join counters;
  print_newline ();;


asdf [1;2;3];;

