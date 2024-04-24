class Splitter(private val limit: Double) {
  private var smallFigures: List[Figure] = List()
  private var largeFigures: List[Figure] = List()

  def apply(figure: Figure): Unit = {
    if (figure.area > limit) {
      largeFigures = largeFigures :+ figure
    } else {
      smallFigures = smallFigures :+ figure
    }
  }

  def showSmallFigures(): Unit = {
    println("Small figures:")
    smallFigures.foreach(f => println("Area: " + f.area))
  }

  def showLargeFigures(): Unit = {
    println("Large figures:")
    largeFigures.foreach(f => println("Area: " + f.area))
  }
}

object Splitter {
  def apply(limit: Double) = new Splitter(limit)
}