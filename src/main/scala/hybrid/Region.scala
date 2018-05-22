package hybrid

import linear.DoubleOps
import linear.Vector
import util.Uniform

case class Region(left: Vector, right: Vector) {
  assert(left.length == right.length)
  assert(left <= right)

  def format = {
    val bounds = for (i <- 0 to dimensions) yield left(i) + " " + right(i)
    bounds.mkString("[", ";", "]")
  }

  for (i <- 0 until dimensions) {
    assert(left(i) > Double.NegativeInfinity)
    assert(right(i) < Double.PositiveInfinity)
  }

  def dimensions = left.length

  def contains(y: Vector) = {
    val n = y.length
    (0 until n) forall {
      i => left(i) <= y(i) && y(i) <= right(i)
    }
  }

  def sample() = {
    Vector(dimensions, i => Uniform.sample(left(i), right(i)))
  }

  def corners = {
    def corners(s: List[Boolean]): Seq[Vector] = {
      if (s.length == dimensions) {
        val x = Vector(s.zipWithIndex map {
          case (p, i) => if (!p) left(i) else right(i)
        }: _*)
        Seq(x)
      } else {
        val k = dimensions - s.length - 1
        if (left(k) == right(k)) corners(false :: s)
        else corners(false :: s) ++ corners(true :: s)
      }
    }

    corners(Nil)
  }

  def split(ps: Seq[Double]): Vector = {
    val x = Vector(ps.zipWithIndex map {
      case (p, i) =>
        left(i) * (1 - p) + right(i) * p
    }: _*)
    x
  }

  def grid(ns: List[Int], ps: List[Double]): Seq[Vector] = ns match {
    case Nil =>
      assert(ps.length == dimensions)
      Seq(split(ps))

    case n :: ns =>
      val k = dimensions - ps.length - 1
      if (left(k) == right(k)) {
        grid(ns, 0 :: ps)
      } else {
        (0 until n) flatMap {
          i => grid(ns, ((i: Double) / (n - 1)) :: ps)
        }
      }
  }

  def grid(n: Int): Seq[Vector] = {
    val ns = 1 to dimensions map (_ => n)
    grid(ns.toList, Nil)
  }
}
