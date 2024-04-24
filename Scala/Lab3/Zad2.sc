def area(a: Double, b: Double)(f: Double => Double)(n: Int) =

  val dx = (b - a) / n

  val st = List.range(1,n).map(i => f((a + i * dx) - (dx / 2))).foldLeft(0.0)((i, sum) => i + sum)
  val s = List.range(1,n-1).map(x => f(a + x * dx)).foldLeft(0.0)((x, sum) => x + sum)

  dx / 6 * (f(a) + f(b) + 2 * s + 4 * st)

area(5.0, 10.0)(x => x * x + 2 * x + 1)(1000)
area(0.0, 1.0)(x => 3 * x * x * x + 2 * x * x + x)(1000)
area(0, 0)(x => 3 * x * x * x + 2 * x * x + x)(1000)
area(1.0, 0.0)(x => 3 * x * x * x + 2 * x * x + x)(1000)
area(0.0, 1.0)(x => x)(1000)
area(0.0, 1.0)(_ => 0)(1000)