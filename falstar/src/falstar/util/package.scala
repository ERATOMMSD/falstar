package falstar

import java.awt.Color
import java.lang.management.ManagementFactory
import java.lang.management.MemoryType

package object util {
  def now() = {
    import java.text.SimpleDateFormat
    import java.util.Date
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm")
    df.format(new Date());
  }

  def time[A](m: String, f: => A) = {
    val start = java.lang.System.currentTimeMillis()
    val r = f
    val end = java.lang.System.currentTimeMillis()
    println(m + ": " + (end - start) + "ms")
    r
  }
  
  def peakMemBytes = {
    val pools = ManagementFactory.getMemoryPoolMXBeans

    var total = 0L
    for (i <- 0 until pools.size) {
      val pool = pools.get(i)
      if (pool.getType == MemoryType.HEAP)
        total += pool.getPeakUsage.getUsed
    }
    total
  }

  implicit class IntOps(val n: Int) extends AnyVal {
    def times[A](f: => A): Seq[A] = {
      for (i <- 0 until n) yield f
    }
  }

  implicit class DoubleOps(val d: Double) extends AnyVal {
    def percent = d / 100.0

    def withinBounds(l: Double, u: Double) = {
      if (d < l) l
      else if (d > u) u
      else d
    }
  }
  
  def darker(c: Color) = {
    new Color(
      Math.max(c.getRed - 10, 0),
      Math.max(c.getGreen - 10, 0),
      Math.max(c.getBlue - 10, 0))
  }

  def mod(a: Double, b: Double) = {
    val r = a % b
    if (r < 0) r + b else r
  }

  def numbers(init: Int) = {
    var counter = init
    () => {
      val res = counter
      counter += 1
      res
    }
  }

  def clamp(x: Double, min: Double, max: Double) = {
    if (x < min) min
    else if (x > max) max
    else x
  }

  def interpolate(x: Double, from: (Double, Double), to: (Double, Double)) = {
    val y = (x - from._1) / (from._2 - from._1)
    to._1 + y * (to._2 - to._1)
  }

  def reportProgress(m: String, i: Int, n: Int) = {
    val p = i * 100 / n
    print("\r" + m + ": " + p + "%")
  }

  def group[A, B](xs: Iterable[(A, B)]): Map[A, Iterable[B]] = {
    val ys = xs.groupBy(_._1)
    ys.mapValues(_.map(_._2))
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def coprimes(d: Int) = {
    for (n <- 1 to d if gcd(n, d) == 1)
      yield (n, d)
  }

  object Set1 {
    def unapply[A](s: A => Boolean) = s match {
      case s: Set[A] if s.size == 1 => Some(s.head)
      case _ => None
    }
  }
}
