object Main {
  def main(args: Array[String]): Unit = {
    val rectangle = Rectangle(3,4)
    val square = Rectangle(5)

    println(rectangle.getA)
    println(rectangle.getB)
    println(rectangle.area)

    rectangle.setA(4)
    println(rectangle.getA)
    println(rectangle.getB)
    println(rectangle.area)

    val splitter = Splitter(10)
    splitter(rectangle)
    splitter(square)
    splitter.showSmallFigures()
    splitter.showLargeFigures()

    //val error = Rectangle(-4)
    val error = Rectangle(1, 0)
  }
}