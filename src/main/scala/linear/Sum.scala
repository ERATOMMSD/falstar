package linear

object Sum {
  def apply(length: Int, elem: Int => Double): Double = {
    var res: Double = 0
    for (i <- 0 until length)
      res += elem(i)
    res
  }

  def apply(length: Int, elem: Int => Vector): Vector = {
    var res: Vector = Vector.zero(length)
    for (i <- 0 until length)
      res += elem(i)
    res
  }

  def apply(length: Int, elem: Int => Matrix): Matrix = {
    var res: Matrix = Matrix.zero(length, length)
    for (i <- 0 until length)
      res += elem(i)
    res
  }
}