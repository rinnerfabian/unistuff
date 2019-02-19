type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

let helpRN l name = match l with
    | [] -> raise (Invalid_file_format name)
    | x::xs ->
        if x <> ""
        then match List.hd xs with 
            | "naughty" -> (x,Naughty)
            | "nice" -> (x,Nice)
            | _ -> raise (Invalid_file_format name)
        else raise (Invalid_file_format name);;
let helpRW l name = match l with
    | [] -> raise (Invalid_file_format name)
    | x::xs ->
        if x <> ""
        then (match (int_of_string_opt (List.hd xs)) with 
            | Some i -> if i >= 0 && i <= 100
                then (x,i)
                else raise (Invalid_file_format name)
            | None -> raise (Invalid_file_format name) )
        else raise (Invalid_file_format name);;
let helpLC l name = match l with
    | [] -> raise (Invalid_file_format name)
    | x::xs ->
        if x <> ""
        then (match (int_of_string_opt (List.hd xs)) with 
            | Some i -> if i > 0 then (x,i) else raise (Invalid_file_format name)
            | None -> raise (Invalid_file_format name) )
        else raise (Invalid_file_format name);;

let helper s hf =
    let infile = open_in s in
    let rec help st name =
        match (try Some (input_line st) with End_of_file -> None) with 
            | None -> close_in st;[]
            | Some (str) -> match (String.split_on_char ':' str) with
                | list -> if List.length list = 2
                then (hf list name)::(help st name)                   
                else raise (Invalid_file_format name)
    in help infile s;;

let read_notes s = helper s helpRN;;
let read_wishlist s = helper s helpRW;;
let load_catalouge s = helper s helpLC;;