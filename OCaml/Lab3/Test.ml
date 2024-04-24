let generateList max =
  let rec geneRec i =
    if i<=max then i :: geneRec (i+.1.)
    else []
  in geneRec 1.
;;

List.fold_left(+.) 0. (List.map (fun i -> ((i *. 2.))) (generateList 5.));;

let rec (>=<) l f =
  match l with
  [] -> []
  | _::t when t = [] -> []
  | h::t -> (f h List.hd t):: (List.tl t >=< f)
;;

let list = [1;2;3;4;5;6]
let f = fun x y -> x;

list >=< f;;