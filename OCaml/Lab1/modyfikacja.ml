let separate(l) = (
  let rec separateRec(l, g, f, temp) = (
    if (temp >= List.hd l) then (*add List.hd l to g and let temp = List.hd l*)
    else (*add List.hd l to f*)

    if (List.tl l <> []) then separateRec(List.tl l, g, f, temp)

    (g, f)
  );;

  let temp = list.hd l
  let g = [List.hd l]
  let f = []

  separateRec(List.tl l, g, f, temp)
);;

