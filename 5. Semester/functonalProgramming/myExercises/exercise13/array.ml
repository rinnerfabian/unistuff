module List = struct 
    include List
    let init n f =
    let rec aux x acc = if x >= 0 then aux (x-1) (f x::acc) else acc
    in aux (n-1) []
end

module Thread = struct
  include Thread

  let tc = ref 0

  let create f a =
    tc := !tc + 1;
    create f a
end

open Thread
open Event

let reset () =
  Thread.tc := 0
let threads_created () =
  !Thread.tc

exception OutOfBounds

module Array = struct

type 'a array = 'a list
type 'a msg =
        | Destroy
        | Size of int channel
        | Set of int * 'a * bool channel
        | Get of int * ('a * bool) channel
        | Resize of int * 'a

type 'a t = 'a msg channel

let make s v = 
    let c = new_channel () in
    let a = List.init s (fun _ -> v) in
    let rec task arr =
        let l = List.length arr in
        match (sync (receive c)) with 
            | Destroy -> ()
            | Size (answer_c) -> (sync (send answer_c l);task arr)
            | Set (pos, elem, answer_c) ->
                if (pos >= l) then (sync (send answer_c false);task arr)
                else let r = let rec help li i el =
                    match li with
                        | [] -> failwith "unreachable"
                        | x::xs -> if i = 0 then el::xs else x::(help xs (i-1) el) in 
                help a pos elem in
                (sync (send answer_c true);task r)
            | Get (pos, answer_c) ->
                if (pos >= l)
                then (sync (send answer_c ((List.hd arr),false));task arr)
                else let elem = let rec help2 li i =
                    match li with
                        | [] -> failwith "unreachable2"
                        | x::xs -> if i = 0 then x else help2 xs (i-1)  in
                    help2 arr pos in
                (sync (send answer_c (elem,true));task arr)
            | Resize (len, elems) ->
                if (len > l)
                then let add = let rec help3 l e = if l = 0
                    then [] else e::help3 (l-1) e in
                    help3 (len-l) elems in
                    task (arr@add)
                else let n = let rec help4 l ar = match ar with
                    | [] -> failwith "unreachable3"
                    | x::xs -> if l = 0 then [] else x::help4 (l-1) ar in
                    help4 len arr in
                task n
             in
    let _ = create task a in
    c

let size a =
    let c = new_channel () in
    sync (send a (Size c));
    sync (receive c);;

let set i v a =
    let c = new_channel () in 
    sync (send a (Set (i,v,c)));
    if (sync (receive c))
    then () else raise OutOfBounds;;

let get i a = 
    let c = new_channel () in
    sync (send a (Get (i,c)));
    match (sync (receive c)) with (a,b) ->
        if b then a else raise  OutOfBounds;;
 
let resize s v a =
    sync (send a (Resize(s,v)));;

let destroy a = sync (send a (Destroy));;

end

(* Tests *)

let a = Array.make 3 0;;
Array.resize 1 1 a; Array.size a = 1 && Array.get 0 a = 0 && threads_created () = 1;;
