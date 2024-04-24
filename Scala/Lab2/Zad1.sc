import scala.annotation.tailrec

def find[A](list: List[A])(searched: A) = {
  @tailrec
  def findRec(list: List[A], result: List[Int], t: Int): List[Int] = {
    list match
      case Nil => result
      case head :: tail if head == searched => findRec(tail, t :: result, t + 1)
      case _ :: tail => findRec(tail, result, t + 1)
  }

  findRec(list, List(), 0)
}

val find123 = find(List(1, 2, 3))
find123(3)
find123(4)

val findAbc = find(List("a", "b", "c", "d", "a", "e", "a", "f", "g", "h"))
findAbc("a")
findAbc("c")

val findEmptyList = find(List())
findEmptyList(3)

find(List(1))(2)
find(List(1))(1)