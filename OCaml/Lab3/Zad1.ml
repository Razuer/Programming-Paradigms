let rec ( >=< ) l f =
  match l with
  [] -> []
  | _::t when t = [] -> []
  | h::t -> (f h (List.hd t)) :: (List.tl t >=< f)
;;

let rec collapse l f =
  match l with
  []->[]
  | _::t when t = [] -> l
  | _ -> collapse (l>=<f) f
;;

[] >=< fun x y -> x + y;;
[1] >=< fun x y -> x - y;;
[1;2] >=< fun x y -> x * y;;
[1.25;2.5;3.75;4.] >=< fun x y -> x *. y;;
["a";"b";"c";"d";"e"] >=< fun x y -> x ^ y;;
[1;2;3;4;5;6] >=< fun x y -> x * y;;
[1;2;3;4;5;6;7;8;9] >=< ( * );;

[1;2;3;4;5;6;7;8;9] >=< ( + ) >=< ( * );;

collapse [] ( + );;
collapse [1] (fun x y -> x * y);;
collapse [1;2] (fun x y -> x * y);;
collapse [1;2;3;4;5;6] (fun x y -> x * y);;
collapse ["a";"b";"c";"d";"e"] (fun x y -> x ^ y);;
collapse [1.25;2.3;3.5;4.6;5.8;6.] (fun x y -> x *. y);;
collapse [1;2;3;4;5;6;7;8;9] (fun x y -> x * y);;