def substitute[A](list: List[A])(target: A)(replacement: A): List[A] = {
  list match
    case Nil => Nil
    case head :: tail if head == target => replacement :: substitute(tail)(target)(replacement)
    case head :: tail => head :: substitute(tail)(target)(replacement)
}

substitute(Nil)(3)(0)

substitute(List(3, 6, 4, 3, 5, 4, 3, 2, 7, 7))(3)(0)

substitute(List(3, 6, 4, 3, 5, 4, 3, 2, 7, 7))(1)(0)

substitute(List("My", "Name", "Is", "Bond", "James", "Bond"))("Bond")("Blunt")

substitute(3 :: 4 :: 5 :: 6 :: 3 :: 5 :: 23 :: 3 :: Nil)(3)(0)

val substituteNumb = substitute(List(3, 6, 4, 3, 5, 4, 3, 2, 7, 7))
substituteNumb(3)(0)

