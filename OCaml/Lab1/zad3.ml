(*let _substitute (l, target, replacement)  = (
  List.map(fun a -> if (a = target) then replacement else a) l
  );;*)

let rec substitute (l,target,replacement) = (
  if (l = []) then []
  else if (List.hd l = target) then replacement :: substitute(List.tl l, target, replacement)
  else List.hd l :: substitute(List.tl l, target, replacement)
);;

(*------- TESTS -------*)

let l = [3; 6; 4; 3; 5; 4; 3; 2; 7; 7];;
substitute(l, 3, 0);;
substitute([], 3, 0);;
substitute([3], 3, 0);;
substitute(["My"; "Name"; "Is"; "Bond"; "James"; "Bond"], "Bond", "Blunt");;
