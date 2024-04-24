let separate list =
  let rec separateRec (l, results, temp) =
    if l = [] then results
    else if List.hd l < temp then separateRec(List.tl l, (fst results, List.hd l::snd results), temp) 
    else separateRec(List.tl l, ((List.hd l :: fst results), snd results), List.hd l)
  in if list = [] then ([],[])
  else separateRec(List.tl list, ([List.hd list],[]), List.hd list)
;;

separate([]);;
separate([1]);;
separate([8;1]);;
separate([1;4;3]);;
separate([1;2;3;4;2;5;6;7;4;8]);;