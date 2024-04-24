let rec swap2(l) = (
  if (l=[]) then []
  else if ((List.tl l) = []) then [List.hd l]
  else (List.hd (List.tl l)) :: (List.hd l) :: swap2(List.tl (List.tl l))
);;

(*------- TESTS -------*)

let a = [];;
let b = [1];;
let c = ["a";"b";"c";"d";"e"];;

let d = [1;2;3;4;5;6;7;8;9];;
let e = [1;2;3;4;5;6;7;8];;
let f = ["raz"; "dwa"; "trzy"; "cztery"];;


swap2 a;;
swap2 b;;
swap2 c;;
swap2 d;;
swap2 e;;
swap2 f;;