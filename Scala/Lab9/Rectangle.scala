class Rectangle(private var a: Double, private var b: Double) extends Figure {
  if (a <= 0 || b <= 0) throw new IllegalArgumentException("Invalid rectangle sides")
  def area: Double = a * b

  def getA: Double = a
  def getB: Double = b

  def setA (newA: Double) : Unit = {
    if (newA <= 0) throw new IllegalArgumentException("Invalid rectangle side")
    else a = newA
  }

  def setB(newB: Double): Unit = {
    if (newB <= 0) throw new IllegalArgumentException("Invalid rectangle side")
    else b = newB
  }
}

object Rectangle {
  def apply(a: Double) = new Rectangle(a, a)
  def apply(a: Double, b: Double) = new Rectangle(a,b)
}
