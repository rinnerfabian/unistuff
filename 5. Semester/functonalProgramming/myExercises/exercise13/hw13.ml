module List = struct 
    include List
    let init n f =
    let rec aux x acc = if x >= 0 then aux (x-1) (f x::acc) else acc
    in aux (n-1) []
end

(* testing utilities [do not change] *)

exception SyncDeadlocked
module Event = struct
  include Event

  let tsync t e =
    let timer = new_channel () in
    let run_timer () =
      Thread.delay t;
      poll (send timer None)
    in
    let _ = Thread.create run_timer () in
    match (select [wrap e (fun x -> Some x); receive timer]) with
    | Some x -> x
    | None -> raise SyncDeadlocked

  let tselect t es =
    tsync t (choose es)

  let sync e = tsync 2. e
  let select es = tselect 2. es
end

module Thread = struct
  include Thread

  let tc = ref 0

  let create f a =
    tc := !tc + 1;
    create f a
end


(*****************************************************************************)
(*************************** START OF HOMEWORK *******************************)
(*****************************************************************************)
open Thread
open Event

(* 13.4 *)
let help_unary f c =
    let s = sync (receive c) in
    match s with 
        | (Some s',None) -> (sync (send c (None,(Some (f s')))))
        | (_,_) -> failwith "unreachable3";;
let par_unary f a =
  let channels = List.init (List.length a) (fun _ -> new_channel ()) in
  let counters = List.map (create (help_unary f)) channels in
  let rec run li ch = match li,ch with
    | [],[] -> []
    | x::xs,c::cs -> (sync (send c (Some x,None));
      match (sync (receive c)) with
        | (None,Some y') -> (y'::run xs cs)
        | (_,_) -> failwith "unreachable2")
    | _,_ -> failwith "unreachable"
  in
  let list = run a channels in 
  List.iter join counters;
  print_newline ();
  list;;
(*****************************************************************************)
let help_binary f c =
    let s = sync (receive c) in
    match s with 
        | ((Some s',Some t'),_) -> (sync (send c ((None,None),(Some (f s' t')))))
        | (_,_) -> failwith "unreachable3";;
let par_binary f a b =
  let channels = List.init (List.length a) (fun _ -> new_channel ()) in
  let counters = List.map (create (help_binary f)) channels in
  let rec run l1 l2 ch = match l1,l2,ch with
    | [],[],[] -> []
    | x::xs,y::ys,c::cs -> (sync (send c ((Some x,Some y),None)));
      (match (sync (receive c)) with
        | ((_,_),Some y') -> (y'::run xs ys cs)
        | _,_ -> failwith "unreachable2")
    | _,_,_ -> failwith "unreachable"
  in
  let list = run a b channels in 
  List.iter join counters;
  print_newline ();
  list;;

(* 13.5 *)
exception OutOfBounds

module Array = struct

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


(* 13.6 *)
exception InvalidOperation

(* type definitions *)
type user = string
type pass = string
type id = int
type text = string
type account = user * pass
type accList = account list
type document = id * text * user * user list
type docList = document list
type message =  AddAcc of user * pass * bool channel
              | Pub of user * pass * text * (id * bool) channel
              | View of user * pass * id * (text * bool) channel
              | AddViewer of user * pass * id * user * bool channel
              | Change of user * pass * id * user * bool channel
type t = document channel

(* help-functions *)
let exc () = raise InvalidOperation;;

let user_exists user aList = match List.assoc_opt user aList with
  | None -> false 
  | Some _ -> true;;

let user_exists user aList = match aList with
  | [] -> false
  | (u',_)::xs -> if user = u' then true else user_exists user xs;;

let rec user_exists2 user ul = match ul with
  | [] -> false
  | x::xs -> if x = user then true else user_exists2 user xs;;

let rec get_doc id dList = match dList with
  | [] -> None
  | (i,t,u,ul)::xs -> if id = i then Some (i,t,u,ul) else get_doc id xs;;

let rec remove_doc id dList = match dList with
  | [] -> []
  | (i,t,u,ul)::xs -> if i = id then xs else (i,t,u,ul)::remove_doc id xs;;

let rec auth u p aList = match (List.assoc_opt u aList) with
  | None -> false
  | Some p' -> if p' = p then true else false;;

let document_server () =
  let c = new_channel () in
    let server_fun dList =
      let rec help dList aList =
        match (sync (receive c)) with
        | AddAcc (user, pass, answer_c) ->
            if (user_exists user aList)
            then (sync (send answer_c false);help dList aList)
            else (sync (send answer_c true);help dList ((user,pass)::aList))
        | Pub (user, pass, document, answer_c) -> 
            if not (auth user pass aList)
            then (sync (send answer_c (0,false));help dList aList)
            else let l = List.length dList in 
              sync (send answer_c (l,true));
              help ((l,document,user,[])::dList) aList
        | View (user, pass, id, answer_c) ->
            if not (auth user pass aList)
            then (sync (send answer_c ("",false));help dList aList)
            else (match (get_doc id dList) with
              | None -> (sync (send answer_c ("",false));help dList aList)
              | Some (i,t,u,ul) ->
                  if (user_exists2 user ul) || (user = u)
                  then (sync (send answer_c (t,true));help dList aList)
                  else (sync (send answer_c ("",false));help dList aList))
        | AddViewer (user, pass, id, user',answer_c) ->
            if not (auth user pass aList)
            then (sync (send answer_c false);help dList aList)
            else (match get_doc id dList with
              | None -> (sync (send answer_c false);help dList aList)
              | Some (i,t,u,ul) ->
                  if user = u
                  then (sync (send answer_c true);help ((i,t,u,(user'::ul))::(remove_doc i dList)) aList)
                  else (sync (send answer_c false);help dList aList))
        | Change (user, pass, id, user',answer_c) ->
            if not (auth user pass aList)
            then (sync (send answer_c false);help dList aList)
            else (match get_doc id dList with
              | None -> (sync (send answer_c false);help dList aList)
              | Some (i,t,u,ul) ->
                  if user = u
                  then (sync (send answer_c true);help ((i,t,user',ul)::(remove_doc i dList)) aList)
                  else (sync (send answer_c false);help dList aList))
        in help dList []
    in let _ = create server_fun []
    in c

let add_account u p t =
let answer_c = new_channel () in
  sync (send t (AddAcc (u,p,answer_c)));
  if (sync (receive answer_c))
  then ()
  else exc ();;
let publish u p d t =
  let answer_c = new_channel () in
  sync (send t (Pub (u,p,d,answer_c)));
  match (sync (receive answer_c)) with (a,b) ->
    if b then a else exc();;
let view u p i t =
  let answer_c = new_channel () in
  sync (send t (View (u,p,i,answer_c)));
  match (sync (receive answer_c)) with (a,b) ->
    if b then a else exc();;
let add_viewer u p i u' t = 
  let answer_c = new_channel () in
  sync (send t (AddViewer (u,p,i,u',answer_c)));
  if (sync (receive answer_c))
  then ()
  else exc ();;
let change_owner u p i u' t = 
  let answer_c = new_channel () in
  sync (send t (Change (u,p,i,u',answer_c)));
  if (sync (receive answer_c))
  then ()
  else exc ();;

(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let reset () =
  Thread.tc := 0
let threads_created () =
  !Thread.tc

let d_server () =
  let s = document_server () in
  add_account "user1" "pass1" s;
  add_account "user2" "pass2" s;
  add_account "user3" "pass3" s;
  s

let tests = [
  (* 13.4 *)
  __LINE_OF__ (fun () -> let pinc = par_unary (fun x -> x + 1) in pinc [8;1;1] = [9;2;2] && threads_created () = 3);
  __LINE_OF__ (fun () -> let psof = par_unary string_of_float in psof [7.;1.] = ["7.";"1."] && threads_created () = 2);
  __LINE_OF__ (fun () -> let pmul = par_binary ( * ) in pmul [1;2;3] [5;6;2] = [5;12;6] && threads_created () = 3);
  __LINE_OF__ (fun () -> let pcon = par_binary ( ^ ) in pcon ["th";"";"ver";"nic"] ["is";"is";"y";"e"] = ["this";"is";"very";"nice"] && threads_created () = 4);
  (* 13.5
  NOTE: Array's functions cannot be tested in isolation, so if a test for size fails it may very well be due to a mistake in your implementation of make *)
  __LINE_OF__ (fun () -> let _ = Array.make 3 "abc" in threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 1. in Array.destroy a; threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.size a = 3);
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in Array.get 0 a = 'x');
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try let _ = Array.get 3 a in false with OutOfBounds -> true);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.set 1 5 a; Array.get 0 a = 0 && Array.get 1 a = 5 && Array.get 2 a = 0 && threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try Array.set 3 'u' a; false with OutOfBounds -> true);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 5 1 a; Array.size a = 5 && Array.get 2 a = 0 && Array.get 3 a = 1 && Array.get 4 a = 1 && threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 1 1 a; Array.size a = 1 && Array.get 0 a = 0 && threads_created () = 1);
  (* 13.6
  NOTE: Document server functions cannot be tested in isolation, so if a test for view fails it may very well be due to a mistake in your implementation of document_server *)
  __LINE_OF__ (fun () -> let _ = document_server () in threads_created () = 1); (* basic thread creation *)
  __LINE_OF__ (fun () -> let s = document_server () in add_account "user1" "pass1" s; true); (* add correct account *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_account "user1" "***" s; false with InvalidOperation -> true); (* account exists already *)
  __LINE_OF__ (fun () -> let s = d_server () in publish "user2" "pass2" "My Document" s <> publish "user1" "pass1" "My Document" s); (* publish document *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = publish "user1" "***" "My Document" s in false with InvalidOperation -> true); (* publish incorrect auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = view "user1" "pass1" 0 s in false with InvalidOperation -> true); (* view invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in "text" = view "user1" "pass1" d s); (* view correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try let _ = view "user2" "pass2" d s in false with InvalidOperation -> true); (* view, no access *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_viewer "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* add viewer invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try add_viewer "user1" "***" d "user3" s; false with InvalidOperation -> (try let _ = view "user3" "pass3" d s in false with InvalidOperation -> true)); (* add viewer invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user2" "pass2" "text" s in add_viewer "user2" "pass2" d "user1" s; view "user1" "pass1" d s = "text"); (* add viewer correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user1" "***" d "user2" s; false with InvalidOperation -> true); (* change owner invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try change_owner "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* change owner invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user2" "pass2" d "user2" s; false with InvalidOperation -> true); (* change owner, not owner *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in change_owner "user1" "pass1" d "user3" s; view "user3" "pass3" d s = "mydoc"); (* change owner correct *)
]

let () =
  let rec input_lines ch =
    (try Some (input_line ch) with _ -> None) (* catch stupid EOF exception *)
    |> function Some line -> line :: input_lines ch | None -> []
  in
  let lines = input_lines (open_in __FILE__) in
  let open List in
  let open Printf in
  let fail l =
    let line = nth lines (l-1) in
    let test = String.sub line 25 (String.length line - 27) in
    printf "test \027[31;m%s\027[0;m (line %d) failed!\n" test l;
  in
  let test (l, t) =
    reset ();
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)





