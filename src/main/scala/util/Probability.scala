package util

import scala.util.Random

import scala.collection.mutable

object Probability {
  // Initialization from java.util.Random,
  // however, report the seed for reproducibility

  // L'Ecuyer, "Tables of Linear Congruential Generators of
  // Different Sizes and Good Lattice Structure", 1999
  var uniquifier = 8682522807148012L
  var _seed: Long = 0
  setUniqueSeed()

  def seed = {
    _seed
  }

  def seed_=(seed: Long) = {
    _seed = seed
    Random.setSeed(seed)
  }
  
  def setUniqueSeed() = {
    uniquifier *= 181783497276652981L
    seed = uniquifier ^ System.nanoTime()
  }
}

object Uniform {
  def sample(): Double = {
    Random.nextDouble()
  }

  def from[A](xs: Seq[A]): A = {
    val i = Random.nextInt(xs.length)
    xs(i)
  }

  def minBy[A](xs: Seq[A])(f: A => Double): A = {
    val fs = xs map f
    val fmin = fs.min
    val ys = (xs, fs).zipped.collect {
      case (x, f) if f <= fmin => x
    }
    from(ys.toSeq)
  }

  def pick[A](xs: mutable.Buffer[A]): A = {
    val i = Random.nextInt(xs.size)
    xs remove i
  }

  def sample(min: Double, max: Double): Double = {
    assert(min <= max)
    val dist = max - min
    min + sample() * dist
  }

  def withProbability(p: Double)(f: => Unit) = {
    val r = sample()
    val ok = r < p
    if (ok) f
    ok
  }

  def choose[A](f: => A, g: => A): A = {
    if (Random.nextBoolean()) f else g
  }

  def choose[A](p: Double)(f: => A, g: => A): A = {
    if (sample() < p) f else g
  }

  def choose[B](fs: (() => B)*): B = {
    val f = from(fs)
    f()
  }

  // make sure we only select from those functions that are applicable
  // return b as default value if none works
  def choose[A, B](default: A => B, fs: (A => B)*): A => B = {
    a =>
      val gs = fs collect {
        case f: PartialFunction[A, B] if f isDefinedAt a => f
        case f => f
      }
      if (!gs.isEmpty) {
        val f = from(gs)
        f(a)
      } else {
        default(a)
      }
  }
}

object Proportional {
  def sample(ps: Double*): Int = {
    val x = Uniform.sample(0, ps.sum)
    val qs = ps.scanLeft(0.0)(_ + _)
    var i = 0
    while (qs(i + 1) < x) { i += 1 }
    i
  }

  def sample[A](xs: Seq[A], ps: Seq[Double]): A = {
    xs(sample(ps: _*))
  }

  def sample[A](xps: (A, Double)*): A = {
    val (xs, ps) = xps.unzip
    sample(xs, ps)
  }

  // XXX: Scala's type system doesn't work well with non-curried
  def sample[A](xs: Seq[A])(p: A => Double): A = {
    sample(xs, xs map p)
  }

  def pick[A](xs: mutable.Buffer[A], ps: Seq[Double]): A = {
    val x = Uniform.sample(0, ps.sum)
    val qs = ps.scanLeft(0.0)(_ + _)
    var i = 0
    while (qs(i + 1) < x) { i += 1 }
    xs remove i
  }

  def pick[A](xs: mutable.Buffer[A])(p: A => Double): A = {
    sample(xs, xs map p)
  }

  def main(args: Array[String]) {
    // test proportional sampling
    val ps = Seq(1.0, 2.0, 0.5)
    val xs = Seq("a", "b", "c")
    val m = scala.collection.mutable.Map[String, Double]()
    for (i <- 1 to 3500) {
      val x = sample(xs, ps)
      if (m contains x) m(x) += 1
      else m(x) = 1
    }
    println(m)
  }
}

object UniformRoundrobin {
  def from[A](xs: Seq[A], k: Int, n: Int): A = {
    val m = xs.size
    if (n - k < m) Uniform.from(xs)
    else xs(k % m)
  }
}

object Gaussian {
  def sample(mean: Double, deviation: Double): Double = {
    Random.nextGaussian() * deviation + mean;
  }

  def sample(mean: Double, min: Double, max: Double): Double = {
    val deviation = (max - min) / 2
    sample(mean, deviation, min, max)
  }

  def sample(mean: Double, deviation: Double, min: Double, max: Double): Double = {
    assert(min < max)
    while (true) {
      val x = sample(mean, deviation)
      if (min <= x && x <= max) return x
    }
    0
  }
}

