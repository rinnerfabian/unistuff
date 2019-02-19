type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

let read_notes s = let rec help st name =
        match (try Some (input_line st) with End_of_file -> None) with 
            | None -> close_in st;[]
            | Some (str) -> match (String.split_on_char ':' str) with
                | list -> if List.length list = 2 then (match list with
                    | [] -> raise (Invalid_file_format name)
                    | x::xs -> if x <> "" then match List.hd xs with 
                        | "naughty" -> (x,Naughty)::(help st name)
                        | "nice" -> (x,Nice)::(help st name)
                        | _ -> raise (Invalid_file_format name)
                        else raise (Invalid_file_format name) )
                else raise (Invalid_file_format name)
        in help (open_in s) s;;

let read_wishlist s = let rec help st name =
        match (try Some (input_line st) with End_of_file -> None) with 
            | None -> (close_in st; [])
            | Some (str) -> match (String.split_on_char ':' str) with
                | list -> if List.length list = 2 then (match list with
                    | [] -> raise (Invalid_file_format name)
                    | x::xs -> if x <> "" then (match (int_of_string_opt (List.hd xs)) with 
                        | Some i -> if i >= 0 && i <= 100
                                    then (x,i)::(help st name)
                                    else raise (Invalid_file_format name)
                        | None -> raise (Invalid_file_format name) )
                        else raise (Invalid_file_format name))
                else raise (Invalid_file_format name)
        in help (open_in s) s;;

let load_catalogue s = let rec help st name =
        match (try Some (input_line st) with End_of_file -> None) with 
            | None -> (close_in st; [])
            | Some (str) -> match (String.split_on_char ':' str) with
                | list -> if List.length list = 2 then (match list with
                    | [] -> raise (Invalid_file_format name)
                    | x::xs -> if x <> "" then (match (int_of_string_opt (List.hd xs)) with 
                        | Some i -> if i > 0 then (x,i)::(help st name) else raise (Invalid_file_format name)
                        | None -> raise (Invalid_file_format name) )
                        else raise (Invalid_file_format name))
                else raise (Invalid_file_format name)
        in help (open_in s) s;;

let write_list s l = let rec help l doc =
    match l with 
    | [] -> close_out doc
    | x::xs -> output_string doc (x ^ "\n");
                help xs doc;
    in help l (open_out s);;

let write_letter s = let outfile = open_out s in
            output_string outfile ("Hello my friend, \n you failed!! Try your best again next year ;)");
            close_out outfile;;

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
