
object Zad1 {
  def main(args: Array[String]): Unit = {
    val emptyList: List[Int] = List()
    assert(emptyList.isEmpty)
    assert(emptyList.length == 0)

    val nonEmptyList: List[String] = List("item1", "item2", "item3")
    assert(nonEmptyList.nonEmpty)
    assert(nonEmptyList.length == 3)
    assert(nonEmptyList(0) == "item1")
    assert(nonEmptyList(1) == "item2")
    assert(nonEmptyList(2) == "item3")

    val list = List(1, 2, 3, 4, 5)
    assert(list.length == 5)
    assert(list(0) == 1)
    assert(list(1) == 2)
    assert(list(2) == 3)
    assert(list(3) == 4)
    assert(list(4) == 5)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)
    val concatenatedList = list1 ::: list2
    assert(concatenatedList == List(1, 2, 3, 4, 5, 6))

    val transformedList = list.map(_ * 2)
    assert(transformedList == List(2, 4, 6, 8, 10))
  }
}


