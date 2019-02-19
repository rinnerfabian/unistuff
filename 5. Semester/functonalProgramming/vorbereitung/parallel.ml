open Thread
open Event

let map_reduce g f l = 
    let help a =
        let c = new_channel () in
        let task () = 
            sync (send c (f a)) in
        let _ = create task () in
    c in
    let r = List.map help l |> List.map (fun c -> sync (receive c)) in
    fold_left g (List.hd l) (List.tl r);;


