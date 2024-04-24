def layZip[A](lazyList: LazyList[A]): LazyList[(A,A)] = {
  lazyList match
    case h1#::h2#::tail => (h1,h2)#::layZip(h2#::tail)
    case _ => LazyList()
}

layZip(LazyList()).force
layZip(LazyList(1)).force
layZip(LazyList(1,2,3,4)).force
layZip(LazyList(1,2,3,4,5,6,7)).force

layZip(LazyList("raz", "dwa", "trzy")).force



def layZip1[A](lazyList: LazyList[A]): LazyList[(A,A)]={
  if lazyList == LazyList() then LazyList()
  else lazyList.zip(lazyList.tail)
}

layZip1(LazyList())
layZip1(LazyList(1)).force
layZip1(LazyList(1,2,3,4,5,6)).force
