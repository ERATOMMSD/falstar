package util

import linear.Vector
import java.util.Arrays
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

object NelderMead {
  def minimize(feval: Vector => Double, x0: Seq[Vector], lb: Vector, ub: Vector, fmin: Double, nmax: Int): (Double, Vector) = {
    val nm = NelderMead(feval, lb, ub, fmin, nmax)
    val x = nm init x0
    nm steps x
    val xi = x minBy (_.score)
    (xi.score, xi.point)
  }
}

case class NelderMead(feval: Vector => Double, lb: Vector, ub: Vector, fmin: Double, nmax: Int) {
  val alpha = 1.0
  val gamma = 2.0
  val rho = 0.5
  val sigma = 0.5

  case class Solution(point: Vector) extends Ordered[Solution] {
    lazy val score = feval(point) // not needed for center and intermediate results

    def +(that: Solution) = Solution(this.point + that.point)
    def -(that: Solution) = Solution(this.point - that.point)
    def *(value: Double) = Solution(this.point * value)
    def /(value: Double) = Solution(this.point / value)

    def compare(that: Solution) = this.score compare that.score

    override def toString = "Solution(" + point + "," + score + ")"
  }

  object comparator extends java.util.Comparator[Solution] {
    def compare(s1: Solution, s2: Solution) = s1 compare s2
    def equals(s1: Solution, s2: Solution) = s1 equals s2
  }

  def init(x: Seq[Vector]): Array[Solution] = {
    Array.tabulate(x.length)(i => Solution(x(i)))
  }

  def center(x: Seq[Solution]): Solution = {
    x.reduce(_ + _) / x.length
  }

  def steps(x: Array[Solution]) {
    var i = 0
    while (x.forall(_.score > fmin) && i < nmax) {
      println("Nelder-Mead round " + i + " with " + x.size + " samples")
      step(x)
      i += 1
    }
  }

  def bound(xi: Solution): Solution = {
    val y = xi.point
    val z = Vector(y.length, i => Math.min(Math.max(y(i), lb(i)), ub(i)))
    Solution(z)
  }

  def step(x: Array[Solution]) {
    val n = x.length - 2
    assert(n >= 0)

    Arrays.sort(x, comparator)

    val xo = center(x.view.init)
    val xr = bound(xo + (xo - x(n + 1)) * alpha)

    // reflexted point xr is best, try expansion
    if (xr < x(0)) {
      val xe = xo + (xr - xo) * gamma
      if (xe < xr) {
        x(n + 1) = xe
      } else {
        x(n + 1) = xr
      }
    } // reflected point xr is ok
    else if (xr < x(n)) {
      x(n + 1) = xr
    } // reflected point is bad, try contraction or shrink
    else /* if(xr > x(n)) */ {
      val xc = xo + (x(n + 1) - xo) * rho
      if (xc < x(n + 1)) {
        x(n + 1) = xc
      } else {
        for (i <- 1 to n + 1)
          x(i) = x(1) + (x(i) - x(1)) * sigma
      }
    }
  }
}