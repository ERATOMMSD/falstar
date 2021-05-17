package falstar.mtl

import falstar.hybrid.Score
import falstar.hybrid.Signal
import falstar.hybrid.State
import falstar.hybrid.Time
import falstar.hybrid.Duration
import falstar.util.Lemire
import falstar.hybrid.Signal.SignalOps
import falstar.hybrid.Signal.TimeSeriesOps
import falstar.hybrid.Input
import scala.collection.mutable.ArrayBuffer

case class Robustness(rs: Array[(Time, Score)]) {
  def downsample(dt: Duration) = {
    Robustness(rs downsample dt)
  }

  def toMatlab = {
    val qs = rs map { case (t,r) => t + " " + r }
    qs.mkString("[", ";", "]")
  }

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
    Robustness(rs takeWhile (_._1 <= T))
  }

  def prefix(min: Double, max: Double): Time = {
    val i = rs.indexWhere { case (t, r) => min <= r && r <= max }
    if (i < 0) 0
    else rs(i)._1
  }

  def unary_! = unary(Robustness.not)
  def and(that: Robustness) = binary(that)(Robustness.and)
  def or(that: Robustness) = binary(that)(Robustness.or)
  def implies(that: Robustness) = binary(that)(Robustness.implies)

  def unary(f: Double => Double): Robustness = {
    Robustness(rs.map { case ((t, s)) => (t, f(s)) })
  }

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
  def unary_! = {
    Value(Robustness.not(upper), Robustness.not(lower))
  }

  def and(that: Value) = {
    Value(Robustness.and(this.lower, that.lower), Robustness.and(this.upper, that.upper))
  }

  def or(that: Value) = {
    Value(Robustness.or(this.lower, that.lower), Robustness.or(this.upper, that.upper))
  }

  def implies(that: Value) = {
    !this or that
  }
}

object Value {
  def known(score: Score) = Value(score, score)
  val unknown = Value(Score.MinValue, Score.MaxValue)
}

object Robustness {
  val empty = Robustness(Array.empty[(Time, Score)])

  def not(s: Score) = -s
  def and(s1: Score, s2: Score) = Math.min(s1, s2)
  def or(s1: Score, s2: Score) = Math.max(s1, s2)
  def implies(s1: Score, s2: Score) = or(not(s1), s2)

  def product(s1: Score, s2: Score) = s1 * s2
  def sum(s1: Score, s2: Score) = s1 + s2 - s1 * s2

  def lt(left: Term, right: Term, t: Time, u: Input, x: State) = {
    apply(right, t, u, x) - apply(left, t, u, x)
  }

  def equal(left: Term, right: Term, t: Time, u: Input, x: State) = {
    if (Math.abs(apply(right, t, u, x) - apply(left, t, u, x)) < Constraint.threshold)
      Score.MaxValue
    else
      Score.MinValue
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
    case LessEqual(left, right) => not(lt(right, left, t, u, x))
    case Equal(left, right) => equal(left, right, t, u, x)
    case NotEqual(left, right) => not(equal(left, right, t, u, x))
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

    case Not(phi) =>
      !apply(phi, us, xs)

    case And(phi, psi) =>
      apply(phi, us, xs) and apply(psi, us, xs)

    case Or(phi, psi) =>
      apply(phi, us, xs) or apply(psi, us, xs)

    case Implies(phi, psi) =>
      apply(phi, us, xs) implies apply(psi, us, xs)

    case Always(from, to, phi) =>
      apply(phi, us, xs) always (from, to)

    case Eventually(from, to, phi) =>
      apply(phi, us, xs) eventually (from, to)
  }

  def collect(phi: Formula, us: Signal, xs: Signal): (Robustness, List[(Formula, Robustness)]) = phi match {
    case prop: Proposition =>
      val rs = apply(prop, us, xs)
      (rs, Nil)

    case Not(phi) =>
      val (rs, log) = collect(phi, us, xs)
      (!rs, (phi -> rs) :: log)

    case And(phi, psi) =>
      val (rs1, log1) = collect(phi, us, xs)
      val (rs2, log2) = collect(psi, us, xs)
      (rs1 and rs2, List(phi -> rs1, psi -> rs2) ++ log1 ++ log2)

    case Or(phi, psi) =>
      val (rs1, log1) = collect(phi, us, xs)
      val (rs2, log2) = collect(psi, us, xs)
      (rs1 or rs2, List(phi -> rs1, psi -> rs2) ++ log1 ++ log2)

    case Implies(phi, psi) =>
      val (rs1, log1) = collect(phi, us, xs)
      val (rs2, log2) = collect(psi, us, xs)
      (rs1 implies rs2, List(phi -> rs1, psi -> rs2) ++ log1 ++ log2)

    case Always(from, to, phi) =>
      val (rs, log) = collect(phi, us, xs)
      (rs always (from, to), (phi -> rs) :: log)

    case Eventually(from, to, phi) =>
      val (rs, log) = collect(phi, us, xs)
      (rs eventually (from, to), (phi -> rs) :: log)
  }

  def bounds(phi: Formula, us: Signal, xs: Signal): Value = phi match {
    case _ if xs.isEmpty =>
      Value.unknown

    case prop: Proposition =>
      val rs = apply(prop, us, xs)
      Value.known(rs.score)

    case Not(phi) =>
      !bounds(phi, us, xs)

    case And(phi, psi) =>
      bounds(phi, us, xs) and bounds(psi, us, xs)

    case Or(phi, psi) =>
      bounds(phi, us, xs) or bounds(psi, us, xs)

    case Implies(phi, psi) =>
      bounds(phi, us, xs) implies bounds(psi, us, xs)

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