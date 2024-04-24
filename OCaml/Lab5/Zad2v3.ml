let modifiedPascalI n =
  if n<0 then [||]
  else
    let oddTab = Array.make (n+1) 0
    and i = ref 1
    in begin
      oddTab.(0) <- 1;
      while !i <= n do
        if (!i mod 2 = 0) then
          let j = ref 1 and x = ref 1 and y = ref 1
          in while !j <= !i do
            if !j = !i then oddTab.(!j) <- (1)
            else (
              if (!j mod 2 = 0) then x := oddTab.(!j)
              else y := oddTab.(!j);
              oddTab.(!j) <- (!x + !y)
              );
            j := !j + 1            
          done;
        else 
          let j = ref 1 and x = ref 1 and y = ref 1
          in while !j <= !i do
            if !j = !i then oddTab.(!j) <- 1
            else (
              if (!j mod 2 = 0) then (
                x := oddTab.(!j);
                oddTab.(!j) <- (!y - !x)
              )
              else (
                y := oddTab.(!j);
                oddTab.(!j) <- (!x - !y)
              )
            );
            j := !j + 1            
          done;
        ;
        i := !i + 1
      done;
    end;
  
    oddTab
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

1 mod 2;;