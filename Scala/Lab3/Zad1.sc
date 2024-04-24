import scala.annotation.tailrec

def hello[A](l: List[A])(f: (A, A) => A): List[A] =
  def helper(l: List[A]): List[A] =
    l match
      case Nil => Nil
      case h :: t => f(h, t.head) :: helper(t.tail)



  @tailrec
  def check(l:List[A]):Boolean =
    l match
      case _ :: t if t == Nil => false
      case _ :: _ :: t if t == Nil => true
      case _ :: t => check(t.tail)

  if check(l) then helper(l)
  else Nil

hello (List(1,2,3,4,5,6)) ((x,y) => x*y)