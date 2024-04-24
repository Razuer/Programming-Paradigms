let find list searched =
  let rec findRec l r t =
    match l with
    | [] -> r
    | head::tail when head = searched -> findRec tail (t::r) (t+1)
    | head::tail -> findRec tail r (t+1)
  in findRec list [] 0
;;

let find123 = find [1; 2; 3];;
find123 3;;
find123 4;;

let findAbc = find ["a"; "b";"c";"d";"a";"e";"a";"f";"g";"h"];;
findAbc "a";;
findAbc "c";;
findAbc "z";;

let findEmptyList = find [];;
findEmptyList 3;;

find [1] 2;;
find [1] 1;;