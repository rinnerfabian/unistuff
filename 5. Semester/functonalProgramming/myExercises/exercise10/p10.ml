
module type Set = sig
  type t
  val to_string : t -> string
end

module type Map = sig
  type key
  type value
  type t

  val empty : t
  val set : key -> value -> t -> t
  val get : key -> t -> value
  val get_opt : key -> t -> value option
  (* val clear : key -> t -> t *)
  val to_string : t -> string
end

(* Assignment 10.1
   1. StringSet *)
module StringSet : Set = struct
  type t = string
  let to_string s = s
end;;

(* 2. OrderedSet *)
module type OrdererSet = sig
  include Set
  val compare : t -> t -> int
end;;
(* ... IntSet module *)
module IntSet : OrdererSet with type t = int = struct
  type t = int
  let to_string = string_of_int
  let compare a b = if a > b then 1 else if a < b then (-1) else 0
end;;

(* 3. functor BTreeMap *)
module BTreeMap (K : OrdererSet) (V : Set) : Map = struct
  (* interner Typ von OrdererSet *)
  type key = K.t
  (* interner Typ von Set, ohne zu wissen welcher *)
  type value = V.t
  type t = Empty | Node of key * value * t * t

  let (>>) a b = (K.compare a b) > 0
  let (<<) a b = (K.compare a b) < 0
  let (<=>) a b = (K.compare a b) = 0

  let empty = Empty
  let rec set k v = function
    | Empty -> Node (k,v,Empty,Empty)
    | Node (k',v',l,r) -> if k << k'
        then Node (k',v',set k v l, r)
        else if k >> k' then Node (k',v',l,set k v r)
        else Node (k',v',l,r)
  let rec get_opt k = function
    | Empty -> None
    | Node (k',v',l,r) -> if k <=> k'
        then Some v'
        else if k << k' then get_opt k l else get_opt k r
  let get k t = match get_opt k t with
      | None -> raise Not_found
      | Some i -> i
  let to_string = function
    | Empty -> ""
    | Node (k,v,l,r) -> K.to_string k ^ " "
end;;

