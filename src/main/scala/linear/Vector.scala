package linear

import scala.util.Random
import java.util.Arrays

class Vector(val length: Int) extends Traversable[Double] {
  assert(length >= 0)

  val data = Array.ofDim[Double](length)

  def foreach[U](f: Double => U) {
    data foreach f
  }

  override def toString = {
    data.mkString("Vector(", ", ", ")")
  }

  def toMatlabRow = {
    data.mkString("[", " ", "]")
  }

  def toMatlabColumn = {
    data.mkString("[", "; ", "]")
  }

  override def equals(that: Any) = that match {
    case that: Vector =>
      Arrays.equals(this.data, that.data)
    case _ =>
      false
  }

  def apply(i: Int): Double = {
    data(i)
  }

  def apply(r: Range): Vector = {
    assert(0 <= r.start && r.last < length)
    Vector(r.length, i => this(r.start + i))
  }

  def update(i: Int, a: Double) = {
    data(i) = a
  }

  def copy = {
    Vector(length, i => this(i))
  }

  def norm = {
    Math.sqrt(this * this)
  }

  def normalize = {
    this / norm
  }

  def unary_- = {
    Vector(length, i => -this(i))
  }

  def sum = {
    Sum(length, i => data(i))
  }

  def mean = {
    sum / length
  }

  def stddev = {
    val mu = mean
    Math.sqrt(Sum(length, (i: Int) => Math.pow(data(i) - mu, 2)) / length)
  }

  def mid(that: Vector) = {
    (this + that) / 2
  }

  def +(that: Vector) = {
    assert(this.length == that.length)
    Vector(length, i => this(i) + that(i))
  }

  def -(that: Vector) = {
    assert(this.length == that.length)
    Vector(length, i => this(i) - that(i))
  }

  def *(that: Double) = {
    Vector(length, i => this(i) * that)
  }

  def /(that: Double) = {
    Vector(length, i => this(i) / that)
  }

  def *(that: Vector) = {
    assert(this.length == that.length)
    Sum(length, (i: Int) => this(i) * that(i))
  }

  def *(that: Matrix) = {
    assert(this.length == that.rows)
    Vector(that.cols, j => Sum(length, k => this(k) + that(k, j)))
  }

  def <=(that: Vector) = {
    assert(this.length == that.length)
    (this.data, that.data).zipped.forall {
      case (a, b) => a <= b
    }
  }

  def ++(that: Vector) = {
    Vector(
      this.length + that.length,
      i =>
        if (i < this.length) this(i)
        else that(i - this.length))
  }

  def pointwise(f: (Double, Double) => Double)(that: Vector) = {
    assert(this.length == that.length)
    Vector(length, i => f(this(i), that(i)))
  }
}

object Vector {
  val empty = {
    Vector(0)
  }

  def apply(length: Int, init: (Int) => Double): Vector = {
    val res = new Vector(length)
    for (i <- 0 until length)
      res.data(i) = init(i)
    res
  }

  def apply(data: Double*): Vector = {
    val length = data.length
    Vector(length, data(_))
  }

  def unapplySeq(x: Vector) = {
    Some(x.data)
  }

  def zero(length: Int) = {
    Vector(length, i => 0)
  }

  def fill(length: Int)(f: => Double) = {
    Vector(length, _ => f)
  }

  def random(length: Int) = {
    Vector(length, i => Random.nextDouble)
  }

  def unit(length: Int, k: Int) = {
    Vector(length, i => if (i == k) 1 else 0)
  }
}
