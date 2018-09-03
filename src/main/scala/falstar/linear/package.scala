package object linear {
  implicit class DoubleOps(a: Double) {
    def *(that: Vector) = { that * a }
    def *(that: Matrix) = { that * a }
  }

  def cov(x: Vector, y: Vector) = {
    x * y - x.mean * y.mean
  }

  def cov(series: Seq[Vector]) = {
    val points = series.transpose
  }
}
