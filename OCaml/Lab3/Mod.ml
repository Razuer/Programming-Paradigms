let reverse l =
  List.fold_left (fun acc x -> x::acc) [] l
;;

reverse [];;
reverse [1];;
reverse [1;2;3;4;5];;
reverse [1;2;3;4;5;6];;
reverse ["a";"b"; "c"; "d"];;