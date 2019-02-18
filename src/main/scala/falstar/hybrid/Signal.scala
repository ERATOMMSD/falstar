package falstar.hybrid

import scala.collection.mutable.ArrayBuffer

import falstar.linear.Vector

object Signal {
  def length(t0: Time, dt: Duration, T: Time): Int = {
    Math.ceil((T - t0) / dt).toInt
  }

  def apply(points: (Time, Vector)*): Signal = {
    points.toArray[(Time, Vector)]
  }

  def apply(steps: Int, f: Int => (Time, Vector)): Signal = {
    Array.tabulate(steps)(f)
  }

  val empty: Signal = {
    Array()
  }

  def point(t0: Time, x0: State): Signal = {
    Array((t0, x0))
  }

  def uniform(t0: Time, dt: Duration, T: Double)(x: => Vector): Signal = {
    val steps = Signal.length(t0, dt, T)
    Signal(steps, i => (t0 + i * dt, x))
  }

  def uniform(t0: Time, dt: Duration, points: Vector*): Signal = {
    Signal(points.length, i => (t0 + dt * i, points(i)))
  }

  def coarse(t0: Time, dt: Duration, T: Double, controlpoints: Int)(x: => Vector): Signal = {
    val steps = Signal.length(t0, dt, T)
    val us = Seq.tabulate(controlpoints)(i => x)
    Signal(steps, i => (t0 + i * dt, us(i * controlpoints / steps)))
  }

  implicit class SignalOps(xs: Signal) {
    def t0: Time = {
      if (xs.isEmpty) 0
      else xs.head._1
    }

    def T: Time = {
      if (xs.isEmpty) 0
      else xs.last._1
    }

    def until(T: Time) = {
      xs takeWhile (_._1 <= T)
    }

    def combine(ys: Signal): Signal = {
      // assert(xs.length == ys.length)
      (xs, ys).zipped.map {
        case ((t, x), (_, y)) =>
          (t, x ++ y)
      }
    }

    def collapse: Signal = {
      val ys = new ArrayBuffer[(Time, Vector)]()
      for (tx <- xs) {
        if (ys.isEmpty || ys.last._2 != tx._2) {
          ys += tx
        }
      }
      ys.toArray
    }

    def toMatlab(T: Time) = {
      val rs = new StringBuilder()

      rs append "["

      for ((t, u) <- xs) {
        rs append t
        for (x <- u) {
          rs append " "
          rs append x
        }
        rs append "; "
      }

      val (_, un) = xs.last
      rs append T
      for (x <- un) {
        rs append " "
        rs append x
      }

      rs append "]"

      rs.toString
    }
  }
}
