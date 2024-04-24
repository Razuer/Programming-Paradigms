let modifiedPascalI n =
  if n<0 then [||]
  else
    let oddTab = Array.make (n+1) 0
    and evenTab = Array.make (n+1) 0
    and i = ref 1
    in let _ = oddTab.(0) <- 1
    and _ = evenTab.(0) <- 1
    in begin
      while !i <= n do
        if (!i mod 2 = 0) then
          let j = ref 1
          in begin
            while !j <= !i do
              evenTab.(!j) <- (oddTab.(!j-1) + oddTab.(!j));
              j := !j + 1            
            done;
          end;
        else 
          let j = ref 1
          in begin
            while !j <= !i do
              oddTab.(!j) <- (evenTab.(!j-1) - evenTab.(!j));
              j := !j + 1
            done;
          end;
        ;
        i := !i + 1
      done;
    end;
  
    if n mod 2 = 0 then evenTab
    else oddTab
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

-1 mod 4;;