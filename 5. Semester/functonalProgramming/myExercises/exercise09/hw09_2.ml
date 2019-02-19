let todo _ = failwith "TODO"

type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

(* 9.3 - Hilfsfunktionen *)
let error n = raise (Invalid_file_format n);;
let helper s hf =
    let infile = open_in s in
    let rec help st name =
        match (try Some (input_line st) with End_of_file -> None) with 
            | None -> close_in st;[]
            | Some (str) -> match (String.split_on_char ':' str) with
                | list -> if List.length list = 2
                then (hf list name)::(help st name)                   
                else error name
    in help infile s;;

(* 9.3 - 1 *)
let rec hilfsF l name = match l with [] -> error name | x::xs -> if x <> ""
        then match List.hd xs with 
            | "naughty" -> (x,Naughty)
            | "nice" -> (x,Nice)
            | _ -> error name
        else error name;;
let read_notes s = helper s hilfsF;;

(* 9.3 - 2 *)
let helpRW l name = match l with [] -> error name | x::xs -> if x <> ""
    then (match (int_of_string_opt (List.hd xs)) with 
        | Some i -> if i >= 0 && i <= 100
            then (x,i)
             else error name
        | None -> error name )
    else error name;;
let read_wishlist s = helper s helpRW;;

(* 9.3 - 3 *)
let helpLC l name = match l with [] -> error name | x::xs -> if x <> ""
        then (match (int_of_string_opt (List.hd xs)) with 
            | Some i -> if i > 0 then (x,i) else error name
            | None -> error name )
        else error name;;
let load_catalogue s = helper s helpLC;;

(* 9.3 - 4 *)
let write_list s l = let rec help l doc =
    match l with 
    | [] -> close_out doc
    | x::xs -> output_string doc (x ^ "\n");
                help xs doc;
    in help l (open_out s);;

(* 9.3 - 5 *)
let write_letter s = let outfile = open_out s in
            output_string outfile ("Hello my friend, \n naughty peace of s**t");
            close_out outfile;;

(* 9.3 - 6 *)
let run_santas_factory (i:int) sa =
    let catalogue = load_catalogue "toys_catalogue.txt" in
    let notes = read_notes "santas_notes.txt" in
    let rec help no = match no with 
        | [] -> ()
        | (na,be)::xs ->
            if be = Naughty
            then (write_letter (na^"_letter.txt"); help xs;)
            else (  let wishlist = read_wishlist (na^"_wishlist.txt") in
                    let return = let rec help2 wl = (match wl with 
                        | [] -> []
                        | (toy,imp)::ys -> let weight = let rec help3 ca = (match ca with 
                            | [] -> None
                            | (t,w)::zs -> if t = toy then Some w else help3 zs)
                            in help3 catalogue in
                            (match weight with 
                                | None -> help2 ys
                                | Some (wei) -> (toy,imp,wei)::help2 ys ))
                    in help2 wishlist in
                    write_list (na ^ "_presents.txt") (sa return i); help xs; )             
    in help notes;;

(* 9.3 - 7 *)
let my_compare (t,i,w) (t',i',w') = if w > w' then 1 else if w < w' then -1 else 0;;
let sort_by_weight l = List.sort my_compare l;;
let initial i w = let list = let rec help i acc = if i = 0 then acc else help (i-1) ([0]::acc)
        in help i []
        in let start = let rec help2 j = if j = (-1) then [] else j::help2 (j-1)
        in help2 w
        in List.rev(start)::list;;
let rec get2 j l = match l with
        | [] -> failwith "unreachable"
        | x::xs -> if j = 0 then x else get2 (j-1) xs;;
let rec get i j l = match l with
        | [] -> failwith "unreachable"
        | x::xs -> if i = 0 then get2 j x else get (i-1) j xs;;
let rec insert e i l = match l with
        | [] -> l
        | x::xs -> if i = 0 then (x@[e])::xs else x::(insert e (i-1) xs) ;;
let max a b = if a > b then a else b;;

let matrix lt w =
    let rec help l w acc i j = match l with 
        | [] -> acc
        | (t,im,we)::xs -> if j > w then help xs w acc (i+1) 1 else 
                if we <= (get 0 j acc)
                then if i = 1
                        then help l w (insert im i acc) i (j+1)
                        else help l w (insert (max (im+(get (i-1)(j-we) acc)) (get (i-1) j acc)) i acc) i (j+1)
                else if i = 1
                        then help l w (insert 0 j acc) i (j+1)
                        else help l w (insert (get (i-1) j acc) i acc) i (j+1)
    in help (sort_by_weight lt) w (initial (List.length lt) w) 1 1;;

let knapsack lt w =
        let rec help l ma i j acc = match l with 
          | [] -> acc
          | (t,im,we)::xs -> match (get i j ma) with z -> if z = 0 then acc
          else if i = 1
            then
              if z = 0
              then help xs ma (i-1) j acc
              else help xs ma (i-1) j (t::acc)
            else 
              if z = (get (i-1) j ma)
              then help xs ma (i-1) j acc
              else help xs ma (i-1) (j-we) (t::acc)
        in help (List.rev(sort_by_weight lt)) (matrix lt w) (List.length lt) w [];;


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a933_ex1 = ["penguin doll",1; "ocaml book",2; "time machine",53; "bike",7; "barbie's dream house",5;
  "guitar",6; "colorful pencils",2; "socks",1; "shawl",2; "karaoke machine",13; "superman action doll set",3;
  "guinea pig",3; "horse",10; "unicorn",8; "sand toys",4; "soccer shoes",3]

(*****************************************************************************)
(* TESTS [do not change] *)
let (=^) a b =
    (List.sort compare a) = (List.sort compare b)
let (=|) a b =
    let a = List.sort_uniq (fun x y -> compare (fst x) (fst y)) a in
    let b = List.sort_uniq (fun x y -> compare (fst x) (fst y)) b in
    a = b
let check_throws e f =
  try f (); false with e' -> e' = e

let check_file filename content =
  let file = open_in filename in
  let rec read acc =
    try
      read ((input_line file)::acc)
    with End_of_file -> acc
  in
  let c = read [] in
  close_in file;
  (List.sort_uniq compare c) = (List.sort_uniq compare content)

let check_letter filename =
  let file = open_in filename in
  let rec read () =
    try
      let line = input_line file in
      if line <> "" then true else
      read ()
    with End_of_file -> false
  in
  let r = read () in
  close_in file;
  r

let raise' = function Failure f ->
  Printf.printf "TEST FAILURE: %s\n" f;
  raise (Failure f)
  | e -> raise e

let check_run_santas_factory () =
  let test_selection_alg wishes capacity =
    if capacity <> 13 then raise' (Failure "wrong capacity passed to selection_alg");
    (match List.find_opt (fun (t,_,_) -> t = "ocaml book") wishes with
    | None -> raise' (Failure "wrong list passed to selection_alg")
    | Some (_,_,w) -> if w <> 2 then raise' (Failure "wrong list passed to selection_alg"));
    match List.sort (fun (_,i,_) (_,j,_) -> compare j i) wishes with
    | (w1,_,_)::(w2,_,_)::_ -> [w1;w2]
    | _ -> raise' (Failure "wrong list passed to selection_alg")
  in
  ignore(run_santas_factory 13 test_selection_alg);
  if not (check_letter "marta_letter.txt") then raise (Failure "no correct letter produced for marta");
  if not (check_letter "bruno_letter.txt") then raise (Failure "no correct letter produced for bruno");
  if not (check_file "frida_presents.txt" ["colorful pencils";"ocaml book"]) then raise (Failure "no correct present list produced for frida");
  if not (check_file "tommy_presents.txt" ["sand toys";"superman action doll set"]) then raise (Failure "no correct present list produced for tommy");
  if not (check_file "caren_presents.txt" ["penguin doll";"unicorn"]) then raise (Failure "no correct present list produced for caren");
  true

let tests = [
  (* tests for 9.3 - 1 *)
  __LINE_OF__ (fun () -> (read_notes "examples/santas_notes.txt") =| ["tommy",Nice;"bruno",Naughty;"frida",Nice;"caren",Nice;"marta",Naughty]);
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  (* tests for 9.3 - 2 *)
  __LINE_OF__ (fun () -> (read_wishlist "examples/frida_wishlist.txt") =| ["ocaml book",10;"horse",3;"colorful pencils",12]);
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  (* tests for 9.3 - 3 *)
  __LINE_OF__ (fun () -> (load_catalogue "examples/toys_catalogue.txt") =| a933_ex1);
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  (* tests for 9.3 - 4 *)
  __LINE_OF__ (fun () -> let l = ["socks";"colorful pencils";"horse"] in let fn = "examples/testout_list1.txt" in write_list fn l; check_file fn l);
  (* tests for 9.3 - 5 *)
  __LINE_OF__ (fun () -> let fn = "examples/testout_letter1.txt" in write_letter fn; check_letter fn);
  (* tests for 9.3 - 6 *)
  __LINE_OF__ (fun () -> check_run_santas_factory ());
  (* tests for 9.3 - 7 *)
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "b",2,2; "b",2,2; "d",4,5; "b",2,2; "e",8,2] 10) =^ ["a";"b";"b";"e"]);
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "a",5,4; "c",11,6; "d",4,5; "e",8,2; "a",5,4] 10) =^ ["c";"e"]);
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
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)









