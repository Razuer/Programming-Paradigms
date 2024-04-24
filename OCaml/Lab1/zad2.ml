let roots (a,b,c) = (
  let delta = (b*.b-.4.*.a*.c) in
  if (delta<0.) then 0
  else if (delta=0.) then 1
  else 2
);;


(*------- TESTS -------*)

roots(1.,1.,1.);;
roots(1.,2.,1.);;
roots(1.,3.,1.);;
roots(0.,0.,3.);;
roots(3.46,-9.234,-34.22);; 