package util

import java.util.ArrayDeque

object Lemire {
  def main(args: Array[String]) = {
    val rs: Array[(Double, Double)] = Array((0, 0.3), (1, 0.2), (2, 0.5), (3, 0.4), (4, 0.3), (5, 0.1))
    val rb = backward(rs, 1, 10, 0, _ > _)
    println(rb.mkString("[", ", ", "]"))
  }

  def forward(xs: Array[(Double, Double)], from: Double, to: Double, z: Double, p: (Double, Double) => Boolean): Array[(Double, Double)] = {
    val delta = to - from
    val pending = new ArrayDeque[(Double, Double)]()
    val window = new ArrayDeque[(Double, Double)]()
    val ys = Array.ofDim[(Double, Double)](xs.length)

    for (i <- 1 until xs.length) {
      val (ti, _) = xs(i)

      pending.addLast(xs(i))

      val (ta, xa) = pending.getFirst

      if (ti + from <= ta) {
        pending.removeLast()

        var done = false
        while (!window.isEmpty && !done) {
          val (tb, xb) = window.getLast
          if (p(xa, xb)) window.removeLast()
          else done = true
        }

        window.addLast((ta, xa))

        done = false
        while (!window.isEmpty && !done) {
          val (tc, xc) = window.getFirst
          if (ta + delta < xc) window.removeFirst()
          else done = true
        }

        val (tc, xc) = window.getFirst

        ys(i) = (ti, xc)
      } else {
        ys(i) = (ti, z)
      }
    }

    ys
  }

  def backward(xs: Array[(Double, Double)], from: Double, to: Double, z: Double, p: (Double, Double) => Boolean): Array[(Double, Double)] = {
    val delta = to - from
    val pending = new ArrayDeque[(Double, Double)]()
    val window = new ArrayDeque[(Double, Double)]()
    val ys = Array.ofDim[(Double, Double)](xs.length)

    for (j <- 1 to xs.length) {
      val i = xs.length - j

      val (ti, _) = xs(i)

      pending.addFirst(xs(i))

      val (ta, xa) = pending.getLast

      if (ti + from <= ta) {
        pending.removeLast()

        var done = false
        while (!window.isEmpty && !done) {
          val (_, xb) = window.getFirst
          if (p(xa, xb)) window.removeFirst()
          else done = true
        }

        window.addFirst((ta, xa))

        done = false
        while (!window.isEmpty && !done) {
          val (tc, _) = window.getLast
          if (ta + delta < tc) window.removeLast()
          else done = true
        }

        val (_, xc) = window.getLast

        ys(i) = (ti, xc)
      } else {
        ys(i) = (ti, z)
      }
    }

    ys
  }
}