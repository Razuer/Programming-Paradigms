def skipTakeL[A](lazyList: LazyList[A]): LazyList[A] = {
  def skipRec(n: Int, i: Int, j: Int, lazyList: LazyList[A]): LazyList[A] =
    (n, i, j, lazyList) match
      case (_, _, _, LazyList()) => LazyList()
      case (n, i, j, h #:: t) =>
        if n == j then h #:: skipRec(n + 1, i + 1, j + (i * 2), t)
        else skipRec(n + 1, i, j, t)

  skipRec(1, 1, 2, lazyList)
}

skipTakeL(LazyList()).force
skipTakeL(LazyList.from(1, 1).take(1)).force
skipTakeL(LazyList.from(1, 1).take(2)).force
skipTakeL(LazyList.from(1, 1).take(3)).force
skipTakeL(LazyList("Raz", "Dwa", "Trzy","Cztery", "Pięć", "Sześć", "Siedem", "Osiem", "Dziewięć")).force
skipTakeL(LazyList.from(1, 1)).take(32).force

