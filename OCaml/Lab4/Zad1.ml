type 'a tree =
  | Leaf
  | Node of 'a * 'a tree
;;

type snapshot =
  | Origin of string
  | Insert of int*string
  | Delete of int*int
  | Move of int*int*int
;;

type resultString =
  | Error
  | Result of string
;;

let get_string = function
  | Result i -> i
  | _ -> failwith "runtime error: expected string"
;;


let insertPos insertion i str =
  let length = String.length str in
    if length >= i && i>=0 then Result(String.sub str 0 i ^ insertion ^ String.sub str i (length-i))
    else Error
;;

let deletePos pos len str =
  let length = String.length str in
    if(pos + len) > length || pos < 0 || len <= 0 then Error
    else Result(String.sub str 0 pos ^ String.sub str (pos+len) (String.length str - len - pos))
;;

let movePos pos len dest str =
  let length = String.length str in
    if pos > length || dest > length || (pos+len) > length || pos < 0 || len <= 0 || dest < 0  then Error
    else let toMove = String.sub str pos len in (insertPos toMove (dest) (get_string (deletePos pos len str)))
;;

(*
let insertPos insertion i str =
  let length = String.length str in
    try Result(String.sub str 0 i ^ insertion ^ String.sub str i (length-i)) with Invalid_argument "String.sub / Bytes.sub" -> Error
;;

let deletePos pos len str =
  try Result(String.sub str 0 pos ^ String.sub str (pos+len) (String.length str - len - pos)) with Invalid_argument "String.sub / Bytes.sub" -> Error
;;

let movePos pos len dest str =
  try let toMove = String.sub str pos len in (insertPos toMove (dest) (get_string (deletePos pos len str))) with Invalid_argument "String.sub / Bytes.sub" -> Error
;;
*)

let rec insert x tree = 
  match tree with
  | Leaf -> Node (x, Leaf)
  | Node (value, child) -> Node (value, insert x child)
;;

let review tree snap =
  let rec reviewRec result t =
    match result with 
    | Error -> Error
    | Result(str) -> match t with
      | Leaf -> result
      | Node (snap, child) -> match snap with
        | Origin (st) -> reviewRec (Result(st)) child
        | Insert (i,st) -> reviewRec (insertPos st i str) child
        | Delete (i, j) -> reviewRec (deletePos i j str) child
        | Move (i,j,k) -> reviewRec (movePos i j k str) child
  in reviewRec (Result"") (insert snap tree)
;;

let rec nodes t =
  match t with
    Leaf -> 0
    | Node(_,t1) -> 1 + nodes t1
;;

let rec order = function
  | Leaf -> []
  | Node(value, child) -> [value] @ order child
;;

let insert1 = insertPos "LOOL" 0 "Wladyslaw";;
let insert2 = insertPos "LOOL" 4 "Wladyslaw";;
let insert3 = insertPos "LOOL" 9 "Wladyslaw";;
let insert4 = insertPos "LOOL" 12 "Wladyslaw";;
let insert5 = insertPos "LOOL" (-1) "Wladyslaw";;

let delete1 = deletePos 7 2 "Wladyslaw";;
let delete2 = deletePos 0 3 "Wladyslaw";;
let delete3 = deletePos 0 9 "Wladyslaw";;
let delete4 = deletePos 4 9 "Wladyslaw";;
let delete5 = deletePos (-3) (-5) "Wladyslaw";;

let move1 = movePos 3 3 5 "Wladyslaw";;
let move2 = movePos 0 3 3 "Wladyslaw";;
let move3 = movePos 5 4 0 "Wladyslaw";;
let move4 = movePos 5 5 0 "Wladyslaw";;
let move5 = movePos (-3) (-3) (-5) "Wladyslaw";;
let move6 = movePos 0 3 10 "Wladyslaw";;


let tree = Node(Origin("ZDZISLAW"),
									Node(Insert(1, "ha"),
										Node(Insert(1, "lol"),
											Node(Insert(3, "bu"),
												Node(Insert(4, "kek"),
												  Node(Insert(5, "meh"),
													  Node(Insert(6, "hi"),
														  Node(Insert(7, "bum"),
															  Node(Delete(4, 1),
																  Node(Move(1, 17, 8),
                                    Node(Insert(8, " <-> "),
																		  Leaf)))))))))))
;;

review tree (Delete(14,2));;
