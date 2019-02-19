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

exception InvalidOperation

open Thread
open Event


(* type definitions *)
type user = string
type pass = string
type id = int
type text = string
type account = user * pass
type accList = account list
type document = id * text * user * user list
type docList = document list
type message =  AddAcc of user * pass 
              | Pub of user * pass * text * id channel
              | View of user * pass * id * text channel
              | AddViewer of user * pass * id * user
              | Change of user * pass * id * user
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
        | AddAcc (user, pass) ->
            if (user_exists user aList) then exc ()
            else help dList ((user,pass)::aList)
        | Pub (user, pass, document, answer_c) -> 
            if not (auth user pass aList)
            then (sync (send answer_c 0);exc ())
            else let l = List.length dList in 
            sync (send answer_c l);
            help ((l,document,user,[])::dList) aList
        | View (user, pass, id, answer_c) ->
            if not (auth user pass aList)
            then (sync (send answer_c "");exc ())
            else (match get_doc id dList with
              | None -> (sync (send answer_c "");exc ())
              | Some (i,t,u,ul) ->
                  if not(user_exists2 user ul) && not(user = user)
                  then (sync (send answer_c "");exc ())
                  else (sync (send answer_c t);help dList aList))
        | AddViewer (user, pass, id, user') ->
            if not (auth user pass aList) then exc ()
            else (match get_doc id dList with
              | None -> exc ()
              | Some (i,t,u,ul) ->
                  if user = u
                  then help ((i,t,u,(user'::ul))::(remove_doc i dList)) aList
                  else exc ())
        | Change (user, pass, id, user') ->
            if not (auth user pass aList) then exc ()
            else (match get_doc id dList with
              | None -> exc ()
              | Some (i,t,u,ul) ->
                  if user = u then help ((i,t,user',ul)::(remove_doc i dList)) aList
                  else exc ())
        in help dList []
    in let _ = create server_fun []
    in c

let add_account u p t =
  sync (send t (AddAcc (u,p)));;
let publish u p d t =
  let answer_c = new_channel () in
  sync (send t (Pub (u,p,d,answer_c)));
  sync (receive answer_c);;
let view u p i t =
  let answer_c = new_channel () in
  sync (send t (View (u,p,i,answer_c)));
  sync (receive answer_c);;
let add_viewer u p i u' t = 
  sync (send t (AddViewer (u,p,i,u')));;
let change_owner u p i u' t = 
  sync (send t (Change (u,p,i,u')));;

let reset () =
  Thread.tc := 0
let threads_created () =
  !Thread.tc

let d_server () =
  let s = document_server () in
  add_account "user1" "pass1" s;
  add_account "user2" "pass2" s;
  add_account "user3" "pass3" s;
  s;;

let _ = document_server () in threads_created () = 1;;
let s = document_server () in add_account "user1" "pass1" s; true;;
let s = d_server () in try add_account "user1" "***" s; false with InvalidOperation -> true;;



(*
let s = d_server () in publish "user2" "pass2" "My Document" s <> publish "user1" "pass1" "My Document" s;;
let s = d_server () in try let _ = publish "user1" "***" "My Document" s in false with InvalidOperation -> true;;
let s = d_server () in try let _ = publish "user1" "***" "My Document" s in false with InvalidOperation -> true;;
let s = d_server () in try let _ = view "user1" "pass1" 0 s in false with InvalidOperation -> true;;
let s = d_server () in let d = publish "user1" "pass1" "text" s in "text" = view "user1" "pass1" d s;;
let s = d_server () in let d = publish "user1" "pass1" "text" s in try let _ = view "user2" "pass2" d s in false with InvalidOperation -> true;;
let s = d_server () in try add_viewer "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true;;
let s = d_server () in let d = publish "user1" "pass1" "text" s in try add_viewer "user1" "***" d "user3" s; false with InvalidOperation -> (try let _ = view "user3" "pass3" d s in false with InvalidOperation -> true);;
let s = d_server () in let d = publish "user2" "pass2" "text" s in add_viewer "user2" "pass2" d "user1" s; view "user1" "pass1" d s = "text";;
let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user1" "***" d "user2" s; false with InvalidOperation -> true;;
let s = d_server () in try change_owner "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true;;
let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user2" "pass2" d "user2" s; false with InvalidOperation -> true;;
let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in change_owner "user1" "pass1" d "user3" s; view "user3" "pass3" d s = "mydoc";;
 
*)