open Thread
open Event

(* Ergebnisse asynchron printen *)
let threads1 l = 
    let f e = let r = let rec help (a,b) = if a = b then a else if a > b then help ((a-b),b) else help (a,(b-a)) in help e in
        let s = Printf.sprintf "Thread %d: %d" (id (self())) r in print_endline s in
    let r = List.map (create f) l in
    List.iter join r;;

(* Ergebnisse asynchron berechnen und zurückgeben *)
let threads2 l = 
    let worker e =
        let c = new_channel () in
        let r = let rec help (a,b) = if a = b then a else if a > b then help ((a-b),b) else help (a,(b-a)) in help e in
        let _ = create (fun () -> sync (send c (r))) () in c in
    List.map (fun c -> sync (receive c)) (List.map worker l);;

(* Ergebnisse synchron printen *)
let spawn_counter e c =
  let rec f e =
    let r = let rec help (a,b) = if a = b then a else if a > b then help ((a-b),b) else help (a,(b-a)) in help e in
    let _ = sync (receive c) in
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) r in
    print_endline s;sync (send c true) in
  let t = create f e in
  (t,c)

let threads3 l =
  let thChan = List.map (fun x -> let c = new_channel () in spawn_counter x c) l in
  let rec run ch = match ch with
    | [] -> ()
    | c::cs -> sync (send c true); let _ =  sync (receive c) in run cs in
  run (List.map (fun (a,b) -> b) thChan);
  List.iter (fun (a,b) -> join a) thChan;
  print_newline ();;

threads3 [(3,6);(4,16)];;