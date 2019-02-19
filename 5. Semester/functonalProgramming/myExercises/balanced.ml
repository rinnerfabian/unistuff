
module type S = sig
    type t
    val size : t -> int
    val show : t -> string
end

module Pair (I1 : S) (I2 : S) : S with type t = I1.t * I2.t = struct
    type t = I1.t * I2.t
    let size t = I1.size (fst t)+ I2.size (snd t)
    let show t = "(" ^ (I1.show (fst t)) ^ ", " ^ (I2.show (snd t)) ^ ")"
end

module AList (I : S) : S with type t = I.t list = struct
    type t = (I.t) list
    let rec size t = match t with [] -> 0 | x::xs -> 1 + size t
    let show t = "[" ^ String.concat "; " (List.map (fun x -> I.show x) t) ^ "]"
end

module Int = struct
    type t = int
    let size x = 1
    let show = string_of_int
end

module IPair = Pair (Int) (Int);;
module IList = AList (Int);;

IPair.show (1,2) = "(1, 2)";;
IPair.size (5,4) = 2;;
IList.show [1;2;3] = "[1; 2; 3]";;
IList.size [4;2;5] = 3;;


