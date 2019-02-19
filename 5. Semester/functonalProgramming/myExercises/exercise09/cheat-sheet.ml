module List = struct 
    include List
    let init n f =
    let rec aux x acc = if x >= 0 then aux (x-1) (f x::acc) else acc
    in aux (n-1) []
end

(* Create Exception *)
exception Corrupt_database_file

(* Records-Type erstellen: *)
type student = {
    name : string;
    id : int;
    grades : (int * float) list
}
(* Record erstellen: *)
let st = {name = "Fabian" ; id = 1; grades = [(1,3.);(2,1.7)] }

(* Load in a file *)
let load_file filename = let file = open_in filename in
    let list = let rec help file l =
        match (try Some (input_line file) with End_of_file -> None) with 
            | None -> l
            | Some x -> help file (l@[x])
    in help file [] in
    close_in file;list;;

(* Print out in a file
    output -> string list
    %s -> string
    %d -> int *)
let print_file output filename = let file = open_out filename in
    let write s = Printf.fprintf file "%s\n" s in 
    List.iter write output;
    close_out file;;

open Thread
open Event

(* Threads: *)
(* chaotic *)
let spawn_counter n =
  let rec count i =
    if i > n then ()
    else let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
    count (i+1) in
  create count 0

let run_counters m n =
  let counters = List.init m (fun _ -> spawn_counter n) in
  List.iter join counters;;

(* synchronized 
let spawn_counter n c =
  let rec count i =
    let _ = sync (receive c) in
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
    if i < n then (sync (send c true); count (i+1))
    else (sync (send c false)) in
  create count 0

let run_counters m n =
  let channels = List.init m (fun _ -> new_channel ()) in
  let counters = List.map (spawn_counter n) channels in
  let rec run = function [] -> ()
    | c::cs -> sync (send c true);
      let chans = if sync (receive c) then cs @ [c] else cs in
      run chans
  in
  run channels;
  List.iter join counters;; *)

let par_unary f a =
  let worker a =
    let c = new_channel () in
    let _ = create (fun () -> sync (send c (f a))) () in
    c
  in
  List.map (fun c -> sync (receive c)) (List.map worker a)

(* Server und Events: *)
type message = ToList of string
              | Quad of int * int channel
type t = message channel

let server () =
  let c = new_channel () in
  let rec server_fun list = 
    match (sync (receive c)) with
        | ToList (s) -> server_fun (s::list)
        | Quad (i,answer_c) ->
            (sync (send answer_c (i*i)));
            server_fun list in
  let _ = create server_fun [] in
  c

let add s t = sync (send t (ToList s));;
let quad i t = let answer_c = new_channel () in
  sync (send t (Quad (i,answer_c)));
  (sync (receive answer_c));;
(*
let s = server ();;
add "Hallo" s;;
quad 5 s;; *)

run_counters 10 10;;










