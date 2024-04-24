let modifiedPascalI n =
  if n<0 then [||]
  else
    let evenTab = ref [|1|]
    and i = ref 1
    in begin
      while !i <= n do
        if (!i mod 2 = 0) then
          evenTab := Array.map2 (+) (Array.append [|0|] !evenTab) (Array.append !evenTab [|0|])
        else 
          evenTab := Array.map2 (-) (Array.append [|2|] !evenTab) (Array.append !evenTab [|0|])
        ;
        i := !i + 1
      done;
    end;
  !evenTab
;;

modifiedPascalI (-5);;
modifiedPascalI 0;;
modifiedPascalI 1;;
modifiedPascalI 2;;
modifiedPascalI 3;;
modifiedPascalI 4;;
modifiedPascalI 5;;
modifiedPascalI 6;;
modifiedPascalI 15;;