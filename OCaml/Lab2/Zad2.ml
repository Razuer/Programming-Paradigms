let rec split2Rec list =
  match list with
  | [] -> ([], [])
  | head :: tail -> let(sublist1, sublist2) = split2Rec tail in (head::sublist2, sublist1)
;;

let split2Tail list =
  let rec split2TailRec l result =
    match l with
    | [] -> result
    (* | head :: tail -> let(s1, s2) = result in split2TailRec tail (head::s2, s1) *)
    | head :: tail -> split2TailRec tail (head::snd result, fst result)
  in split2TailRec list ([], [])
;;

split2Rec [];;
split2Rec [1];;
split2Rec [1;2];;
split2Rec [1;2;3;4;5;6;7];;
split2Rec [1;2;3;4;5;6;7;8];;
split2Rec ['a'; 'b'; 'c'];;
split2Rec ["a"; "b"; "c"; "d"; "e"; "f"];;

split2Tail [];;
split2Tail [1];;
split2Tail [1;2];;
split2Tail [1;2;3;4;5;6;7];;
split2Tail [1;2;3;4;5;6;7;8];;
split2Tail ['a'; 'b'; 'c'];;
split2Tail ["a"; "b"; "c"; "d"; "e"; "f"];;