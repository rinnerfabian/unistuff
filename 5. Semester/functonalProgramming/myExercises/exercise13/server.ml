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
            else (match (get_doc id dList) with
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

let s = d_server () in let d = publish "user2" "pass2" "text" s in add_viewer "user2" "pass2" d "user1" s; view "user1" "pass1" d s = "text";;