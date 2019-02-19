type 'a llist = Cons of 'a * (unit -> 'a llist);;
let rec lnat i = Cons (i, (fun () -> lnat (i+1)));;