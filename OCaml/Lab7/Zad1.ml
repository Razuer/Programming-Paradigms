module type COMPARATOR = 
sig
  type t
  type comparison = Less | Equal | Greater
  val compare : t -> t -> comparison
end

module IntComparator : COMPARATOR with type t = int = 
struct
  type t = int
  type comparison = Less | Equal | Greater
  let compare x y =
    if x < y then Less
    else if x > y then Greater
    else Equal
end

module FloatComparator : COMPARATOR with type t = float = 
struct
  type t = float
  type comparison = Less | Equal | Greater
  let compare x y =
    if x < y then Less
    else if x > y then Greater
    else Equal
end

module type PRIORITY_QUEUE  = sig
  type element
  type t

  val create : t
  val isEmpty : t -> bool
  val insert : t -> element -> t
  val retrieve : t -> element
  val remove : t -> t
end

module PriorityQueueImpl (C : COMPARATOR)  = struct
  type element = C.t
  type t = Empty
    | Node of element * t * t

  let create = Empty

  let isEmpty queue =
    match queue with
    | Empty -> true
    | Node _ -> false

  let insert queue element =
    let rec insertRec element queue =
      match queue with
      | Empty -> Node (element, Empty, Empty)
      | Node (e, left, right) ->
          if C.compare element e = C.Less then
            Node (element, insertRec e right, left)
          else
            Node (e, insertRec element right, left)
    in
    insertRec element queue

  let retrieve = function
    | Empty -> failwith "Empty queue"
    | Node (e, _, _) -> e

  let remove queue =
    match queue with
    | Empty -> Empty
    | Node (_, left, right) ->
        let rec merge left right =
          match (left, right) with
          | (Empty, _) -> right
          | (_, Empty) -> left
          | (Node (l, ll, lr), Node (r, rl, rr)) ->
              if C.compare l r = C.Less then
               Node (l, ll, merge lr right)
              else
                Node (r, rl, merge left rr)
        in merge left right
end

module PriorityQueue (C : COMPARATOR) : PRIORITY_QUEUE with type element = C.t = 
struct
  module PQ = PriorityQueueImpl(C)

  type element = PQ.element
  type t = PQ.t

  let create = PQ.create
  let isEmpty = PQ.isEmpty
  let insert = PQ.insert
  let retrieve = PQ.retrieve
  let remove = PQ.remove
end
;;

IntComparator.compare 1 5;;
IntComparator.compare 5 5;;
IntComparator.compare 9 5;;

FloatComparator.compare (1.4) (1.6);;
FloatComparator.compare 1.4 1.4;;
FloatComparator.compare 1.4 1.111;;

module IntPQ = PriorityQueue(IntComparator);;
let queue = IntPQ.create;;
let queue = IntPQ.insert queue 2;;
let queue = IntPQ.insert queue 5;;
let queue = IntPQ.insert queue 9;;
let queue = IntPQ.insert queue 3;;
let queue = IntPQ.insert queue 4;;
let queue = IntPQ.insert queue 7;;
let queue = IntPQ.insert queue 1;;

IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;
let queue = IntPQ.remove queue;;
IntPQ.retrieve queue;;