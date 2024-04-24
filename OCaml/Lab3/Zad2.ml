let area (a, b) f n =

  let generateList max =
    let rec geneRec i =
      if i<=max then i :: geneRec (i+.1.)
      else []
    in geneRec 1.

  and dx = ((b-.a) /. n)
  
  in let st = List.fold_left (+.) 0. (List.map (fun i -> f((a +. i *. dx) -. (dx /. 2.))) (generateList n))
  and s = List.fold_left (+.) 0. (List.map (fun i -> f(a +. i *. dx)) (generateList (n-.1.)))
  in 
  (dx /. 6.) *. (f(a) +. f(b) +. 2. *. s +. 4. *. st)
;;

area (5., 10.) (fun(x) -> (x *. x +. 2. *. x +. 1.)) 100.;;
area (0., 1.) (fun(x) -> (3.*.x*.x*.x +. 2.*.x*.x+.x)) 100.;;
area (0., 0.) (fun(x) -> (3.*.x*.x*.x +. 2.*.x*.x+.x)) 100.;;
area (1., 0.) (fun(x) -> (3.*.x*.x*.x +. 2.*.x*.x+.x)) 100.;;

area (0., 2.) (fun(x) -> (x*.x)) 100.;;
area (-2., 0.) (fun(x) -> (x*.x)) 100.;;

area (0., 1.) (fun(x) -> (x)) 100.;;
area (0., 1.) (fun(x) -> (0.)) 100.;;
area (0., 1.) (fun(x) -> (1.)) 100.;;