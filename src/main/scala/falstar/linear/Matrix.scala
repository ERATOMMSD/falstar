package falstar.linear

import scala.util.Random
import java.util.Arrays

class Matrix(val rows: Int, val cols: Int) {
  val data = Array.ofDim[Double](rows, cols)

  override def toString = {
    val rs = data.map(_.mkString("Vector(", ", ", ")"))
    rs.mkString("Matrix(", "\n", ")")
  }

  def toMatlab = {
    val rs = data.map(_ mkString " ")
    rs.mkString("[", "; ", "]")
  }

  override def equals(that: Any) = that match {
    case that: Matrix if this.data.length == that.data.length =>
      (this.data, that.data).zipped.forall {
        case (a, b) =>
          Arrays.equals(a, b)
      }
    case _ =>
      false
  }

  def apply(i: Int, j: Int) = {
    data(i)(j)
  }

  def update(i: Int, j: Int, a: Double) = {
    data(i)(j) = a
  }

  def copy = {
    Matrix(rows, cols, (i, j) => this(i, j))
  }

  def unary_- = {
    Matrix(rows, cols, (i, j) => -this(i, j))
  }

  def +(that: Matrix) = {
    assert(this.rows == that.rows)
    assert(this.cols == that.cols)
    Matrix(rows, cols, (i, j) => this(i, j) + that(i, j))
  }

  def -(that: Matrix) = {
    assert(this.rows == that.rows)
    assert(this.cols == that.cols)
    Matrix(rows, cols, (i, j) => this(i, j) - that(i, j))
  }

  def *(that: Double) = {
    Matrix(rows, cols, (i, j) => this(i, j) * that)
  }

  def /(that: Double) = {
    Matrix(rows, cols, (i, j) => this(i, j) / that)
  }

  def *(that: Vector) = {
    assert(this.cols == that.length)
    Vector(this.rows, i => Sum(cols, j => this(i, j) + that(j)))
  }

  def *(that: Matrix) = {
    assert(this.cols == that.rows)
    Matrix(this.rows, that.cols, (i, j) => Sum(cols, k => this(i, k) + that(k, j)))
  }

  def t = {
    Matrix(cols, rows, (i, j) => this(j, i))
  }

  def pointwise(f: (Double, Double) => Double)(that: Matrix) = {
    assert(this.cols == that.cols)
    assert(this.rows == that.rows)
    Matrix(rows, cols, (i, j) => f(this(i, j), that(i, j)))
  }
}

object Matrix {
  def apply(rows: Int, cols: Int, init: (Int, Int) => Double): Matrix = {
    val res = new Matrix(rows, cols)
    for (i <- 0 until rows; j <- 0 until cols)
      res.data(i)(j) = init(i, j)
    res
  }

  def rows(data: Vector*): Matrix = {
    assert(!data.isEmpty)
    val rows = data.length
    val cols = data(0).length
    Matrix(rows, cols, data(_)(_))
  }

  def apply(data: Array[Double]*): Matrix = {
    assert(!data.isEmpty)
    val rows = data.length
    val cols = data(0).length
    Matrix(rows, cols, data(_)(_))
  }

  def unapplySeq(A: Matrix) = {
    Some(A.data)
  }

  def zero(rows: Int, cols: Int) = {
    Matrix(rows, cols, (i, j) => 0)
  }

  def random(rows: Int, cols: Int) = {
    Matrix(rows, cols, (i, j) => Random.nextDouble)
  }

  def id(size: Int) = {
    Matrix(size, size, (i, j) => if (i == j) 1 else 0)
  }
}

