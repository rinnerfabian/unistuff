module type Iter = sig
type 'a t
type 'a s
val init : 'a t -> 'a s
val next : 'a s -> 'a option * 'a s
end;;

module ListIter : Iter with type 'a t = 'a list = struct
    type 'a t = 'a list
    type 'a s = 'a list
    let init t = t
    let next = function [] -> (None,[])
        | x::xs -> ((Some x), xs)
end

module TreeIter : Iter = struct
    type 'a t = Leaf | Node of 'a * 'a t * 'a t
    type 'a s = 'a list
    let rec init = function Leaf -> []
        | Node (a,l,r) -> (init l)@[a]@(init r)
    let next = function [] -> (None,[])
        | x::xs -> ((Some x), xs)
end

module type NIter = sig
    include Iter
    val next_filtered : ('a -> bool) -> 'a s -> 'a option * 'a s
    val next_mapped : ('a -> 'b) -> 'a s -> 'b option * 'a s
end

module ExtIter (I : Iter) : NIter = struct
    type 'a t = 'a I.t
    type 'a s = 'a I.s
    let init t = I.init t
    let next s = I.next s

    let rec next_filtered f s = match (next s) with (a,b)
        -> match a with
            | None -> (None,b)
            | Some a' -> if (f a') then (a,b) else next_filtered f b
    let rec next_mapped f s = match (next s) with (a,b)
        -> match  a with 
            | None -> (None,b)
            | Some a' -> ((Some (f a')),b)
end

module type PIter = sig
    type ('a, 'b) t
    type ('a, 'b) s
    val init : ('a, 'b) t -> ('a, 'b) s
    val next : ('a, 'b) s -> ('a * 'b) option * ('a, 'b) s
end

module PairIter (I1 : Iter) (I2 : Iter) : PIter = struct
    type ('a, 'b) t = (('a I1.t) * ('b I2.t))
    type ('a, 'b) s = (('a I1.s) * ('b I2.s))
    let init t = match t with (a,b) -> (I1.init a,I2.init b)
    let next s =  match s with (a,b) ->
        match (I1.next a,I2.next b) with
            | (Some x, xs),(Some y,ys) -> Some (x,y),(xs,ys)
            | (None,xs),(_,ys) -> None,(xs,ys)
            | (_,xs),(None,ys) -> None,(xs,ys)
end



