open Thread
open Event

module Future = struct

    type 'a msg = Result of 'a | Ex of exn
    type 'a t = 'a msg channel

    let create f a =
        let c = new_channel () in
        let task () =
            let r = (try Result (f a) with e -> Ex e) in
            sync (send c r) in
        let _ = create task () in
        c

    let get fut = match sync (receive fut) with
        | Result r -> r
        | Ex e -> raise e

    let then_ f fut =
        let c = new_channel () in
        let task () =
            match get fut with
                | Result r -> sync (send c (f r))
                | Ex e -> sync (send c (Ex e)) in
        let _ = create task () in
        c

end

let f = Future.create (fun x -> x+1) 1;;
Future.get (Future.then_ string_of_int f);;



