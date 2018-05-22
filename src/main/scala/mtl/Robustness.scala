package mtl

import hybrid.Score
import hybrid.Signal
import hybrid.State
import hybrid.Time
import util.Lemire
import hybrid.Signal.SignalOps
import hybrid.Input
import scala.collection.mutable.ArrayBuffer

case class Robustness(rs: Array[(Time, Score)]) {
  import Robustness.RobustnessOps

  def score = {
    if (rs.isEmpty) Score.MaxValue
    else rs.head._2
  }

  def isEmpty = {
    rs.isEmpty
  }

  def t0: Time = {
    if (rs.isEmpty) 0
    else rs.head._1
  }

  def T: Time = {
    if (rs.isEmpty) 0
    else rs.last._1
  }

  def until(T: Time) = {
    Robustness(rs until T)
  }

  def prefix(min: Double, max: Double): Time = {
    val i = rs.indexWhere { case (t, r) => min <= r && r <= max }
    if (i < 0) 0
    else rs(i)._1
  }

  def and(that: Robustness) = binary(that)(Robustness.and)
  def or(that: Robustness) = binary(that)(Robustness.or)

  def product(that: Robustness) = binary(that)(Robustness.product)
  def sum(that: Robustness) = binary(that)(Robustness.sum)

  def binary(that: Robustness)(f: (Double, Double) => Double): Robustness = {
    Robustness((this.rs, that.rs).zipped.map { case ((t, s1), (_, s2)) => (t, f(s1, s2)) })
  }

  def always(from: Time, to: Time): Robustness = {
    Robustness(Lemire.backward(rs, from, to, Score.MaxValue, _ < _))
  }

  def eventually(from: Time, to: Time): Robustness = {
    Robustness(Lemire.backward(rs, from, to, Score.MinValue, _ > _))
  }
}

case class Value(lower: Score, upper: Score) {
  def and(that: Value) = {
    Value(Math.min(this.lower, that.lower), Math.min(this.upper, that.upper))
  }

  def or(that: Value) = {
    Value(Math.max(this.lower, that.lower), Math.max(this.upper, that.upper))
  }
}

object Value {
  def known(score: Score) = Value(score, score)
  val unknown = Value(Score.MinValue, Score.MaxValue)
}

object Robustness {
  val empty = Robustness(Array.empty[(Time, Score)])

  implicit class RobustnessOps(rs: Array[(Time, Score)]) {
    def t0: Time = {
      if (rs.isEmpty) 0
      else rs.head._1
    }

    def T: Time = {
      if (rs.isEmpty) 0
      else rs.last._1
    }

    def until(T: Time) = {
      rs takeWhile (_._1 <= T)
    }
  }

  def and(s1: Score, s2: Score) = Math.min(s1, s2)
  def or(s1: Score, s2: Score) = Math.max(s1, s2)

  def product(s1: Score, s2: Score) = s1 * s2
  def sum(s1: Score, s2: Score) = s1 + s2 - s1 * s2

  def lt(left: Term, right: Term, t: Time, u: Input, x: State) = {
    apply(right, t, u, x) - apply(left, t, u, x)
  }

  def le(left: Term, right: Term, t: Time, u: Input, x: State) = {
    apply(right, t, u, x) - apply(left, t, u, x) + Constraint.threshold
  }

  def equal(left: Term, right: Term, t: Time, u: Input, x: State) = {
    if (Math.abs(apply(right, t, u, x) - apply(left, t, u, x)) < Constraint.threshold)
      Score.MaxValue
    else
      Score.MinValue
  }

  def inequal(left: Term, right: Term, t: Time, u: Input, x: State) = {
    if (Math.abs(apply(right, t, u, x) - apply(left, t, u, x)) < Constraint.threshold)
      Score.MinValue
    else
      Score.MaxValue
  }

  def apply(tm: Term, t: Time, u: Input, x: State): Double = tm match {
    case Const(value) => value
    case InPort(name, index) => u(index)
    case OutPort(name, index) => x(index)
    case Plus(left, right) => apply(left, t, u, x) + apply(right, t, u, x)
    case Minus(left, right) => apply(left, t, u, x) - apply(right, t, u, x)
    case Times(left, right) => apply(left, t, u, x) * apply(right, t, u, x)
    case DividedBy(left, right) => apply(left, t, u, x) / apply(right, t, u, x)
    case Transform(tm, f, _) => f(apply(tm, t, u, x))
  }

  def apply(prop: Proposition, t: Time, u: Input, x: State): Score = prop match {
    case False => Score.MinValue
    case True => Score.MaxValue
    case Less(left, right) => lt(left, right, t, u, x)
    case LessEqual(left, right) => le(left, right, t, u, x)
    case Equal(left, right) => equal(left, right, t, u, x)
    case NotEqual(left, right) => inequal(left, right, t, u, x)
  }

  def apply(prop: Proposition, us: Signal, xs: Signal): Robustness = {
    var i = 1
    var j = 1
    val rs = new ArrayBuffer[(Time, Score)]

    var t = 0
    var (0, u) = us(0)
    var (0, x) = xs(0)

    val r = apply(prop, t, u, x)
    rs += ((t, r))

    while (i < us.length && j < xs.length) {
      val (ti, ui) = us(i)
      val (tj, xj) = xs(j)

      if (ti <= tj) { i += 1; u = ui }
      if (tj <= ti) { j += 1; x = xj }

      var t = Math.min(ti, tj)
      val r = apply(prop, t, u, x)
      rs += ((t, r))
    }

    while (i < us.length) {
      val (t, u) = us(i)
      val r = apply(prop, t, u, x)
      rs += ((t, r))
      i += 1
    }

    while (j < xs.length) {
      val (t, x) = xs(j)
      val r = apply(prop, t, u, x)
      rs += ((t, r))
      j += 1
    }

    Robustness(rs.toArray)
  }

  def apply(phi: Formula, us: Signal, xs: Signal): Robustness = phi match {
    case prop: Proposition =>
      apply(prop, us, xs)

    case And(phi, psi) =>
      apply(phi, us, xs) and apply(psi, us, xs)

    case Or(phi, psi) =>
      apply(phi, us, xs) or apply(psi, us, xs)

    case Always(from, to, phi) =>
      apply(phi, us, xs) always (from, to)

    case Eventually(from, to, phi) =>
      apply(phi, us, xs) eventually (from, to)
  }

  def bounds(phi: Formula, us: Signal, xs: Signal): Value = phi match {
    case _ if xs.isEmpty =>
      Value.unknown

    case prop: Proposition =>
      val rs = apply(prop, us, xs)
      Value.known(rs.score)

    case And(phi, psi) =>
      bounds(phi, us, xs) and bounds(psi, us, xs)

    case Or(phi, psi) =>
      bounds(phi, us, xs) or bounds(psi, us, xs)

    case Always(from, to, _) =>
      val rs = apply(phi, us, xs)
      if (xs.T < from) {
        Value.unknown
      } else if (xs.T < to) {
        Value(Score.MinValue, rs.score)
      } else {
        Value.known(rs.score)
      }

    case Eventually(from, to, _) =>
      val rs = apply(phi, us, xs)
      if (xs.T < from) {
        Value.unknown
      } else if (xs.T < to) {
        Value(rs.score, Score.MaxValue)
      } else {
        Value.known(rs.score)
      }
  }
}