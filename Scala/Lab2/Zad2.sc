import scala.annotation.tailrec

def split2Rec[A](list: List[A]): (List[A], List[A]) = {
  list match
    case Nil => (Nil, Nil)
    case head :: tail =>
      val (s1, s2) = split2Rec(tail)
      (head :: s2, s1)
}

def split2Tail[A](list: List[A]): (List[A], List[A]) = {
  @tailrec
  def split2TailRec(l: List[A], result: (List[A], List[A])): (List[A], List[A]) = {
    l match
      case Nil => result
      case head :: tail =>
        //val (s1, s2) = result
        //split2TailRec(tail, (head::s2, s1))
        split2TailRec(tail, (head :: result._2, result._1))
  }

  split2TailRec(list, (Nil, Nil))
}

split2Rec(Nil)
split2Rec(List(1))
split2Rec(List(1, 2))
split2Rec(List(1, 2, 3, 4, 5, 6, 7))
split2Rec(List(1, 2, 3, 4, 5, 6, 7, 8))
split2Rec(List('a', 'b', 'c'))
split2Rec(List("a", "b", "c", "d", "e", "f"))

split2Tail(Nil)
split2Tail(List(1))
split2Tail(List(1, 2))
split2Tail(List(1, 2, 3, 4, 5, 6, 7))
split2Tail(List(1, 2, 3, 4, 5, 6, 7, 8))
split2Tail(List('a', 'b', 'c'))
split2Tail(List("a", "b", "c", "d", "e", "f"))