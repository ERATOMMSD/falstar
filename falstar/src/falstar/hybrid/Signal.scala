package falstar.hybrid

import scala.collection.mutable.ArrayBuffer

import falstar.linear.Vector

object Signal {
  def main(args: Array[String]) {
    val xs = Array((0.0, 'a'), (1.0, 'b'), (2.0, 'c'))
    val ys = xs.sample(1.0, 3)
    ys foreach println
  }

  def parse(str: String): Signal = {
    for(x <- falstar.util.splitMatlab2(str)) yield {
      val t = x(0).toDouble
      val y = Vector(x.length - 1, i => x(i + 1).toDouble)
      (t, y)
    }
  }

  def parse(times: String, points: String): Signal = {
    val ts = falstar.util.splitMatlab1(times)
    val xs = falstar.util.splitMatlab2(points)
    assert(ts.length == xs.length)
    for((t,x) <- (ts zip xs)) yield {
      val y = Vector(x.length, i => x(i).toDouble)
      (t.toDouble, y)
    }
  }

  def zip(ts: Vector, xs: Seq[Vector]): Signal = {
    assert(ts.length == xs.length)
    ts.data zip xs
  }

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

  implicit class TimeSeriesOps[A](xs: Array[(Time, A)]) {
    def downsample(dt: Duration) = {
      var ct: Time = 0
      for((t, a) <- xs if t >= ct) yield {
        ct += dt
        (t, a)
      }
    }

    def sample(dt: Duration, T: Time) = {
      var ct: Time = 0
      var i = 0
      var a = xs(0)._2
      val res = new ArrayBuffer[(Time, A)]()

      while(ct <= T + 1e-4) {
        while(i < xs.length && xs(i)._1 <= ct + 1e-4) { a = xs(i)._2; i += 1 }
        res += (ct -> a)
        ct += dt
      }

      res.toArray
    }
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

    def sample(dt: Duration) = {

    }

    def toMatlab(T: Time) = {
      val rs = new StringBuilder()

      rs append "["

      var first = true
      for ((t, u) <- xs) {
        if(!first)
          rs append "; "
        first = false
        rs append t
        for (x <- u) {
          rs append " "
          rs append x
        }
      }

      // val (_, un) = xs.last
      // rs append T
      // for (x <- un) {
      //   rs append " "
      //   rs append x
      // }

      rs append "]"

      rs.toString
    }
  }
}
