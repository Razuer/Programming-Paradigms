let modifiedPascalF n =
  if n>=0 then
    let nextRow i row =
      if (i mod 2 = 0) then List.map2 (+) ([0] @ row) (row @ [0]) 
      else List.map2 (-) ([2] @ row) (row @ [0]) 

    in let rec pascalRec i row =
      if i = n then row
      else pascalRec (i+1) (nextRow (i+1) row)

    in pascalRec (0) [1]

  else []
;;

 modifiedPascalF (-5);;
 modifiedPascalF (0);;
 modifiedPascalF (1);;
 modifiedPascalF (2);;
 modifiedPascalF (3);;
 modifiedPascalF (4);;
 modifiedPascalF (5);;
 modifiedPascalF (6);;
 modifiedPascalF (15);;