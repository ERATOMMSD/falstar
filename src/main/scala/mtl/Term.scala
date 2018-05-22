package mtl

sealed trait Term {
  def dim: Int

  def +(that: Term): Term = Plus(this, that)
  def -(that: Term): Term = Minus(this, that)
  def *(that: Term): Term = Times(this, that)
  def /(that: Term): Term = DividedBy(this, that)

  def <(that: Term): Constraint = Less(this, that)
  def >(that: Term): Constraint = Less(that, this)
  def <=(that: Term): Constraint = LessEqual(this, that)
  def >=(that: Term): Constraint = LessEqual(that, this)
  def ===(that: Term): Constraint = Equal(this, that)
  def !==(that: Term): Constraint = NotEqual(this, that)

  def ~=(that: Term): Formula = (this >= that) && (this <= that)
  def in(lb: Term, ub: Term): Formula = lb <= this && this <= ub
}

case class Const(value: Double) extends Term {
  def dim = 0
  override def toString = value.toString
}

sealed trait Port extends Term {
  def name: String
}

case class InPort(name: String, index: Int) extends Port {
  def dim = index + 1
  override def toString = name
}

case class OutPort(name: String, index: Int) extends Port {
  def dim = index + 1
  override def toString = name
}

sealed trait Arithmetic extends Term {
  def left: Term
  def right: Term
  def dim = Math.max(left.dim, right.dim)
}

case class Plus(left: Term, right: Term) extends Arithmetic {
  override def toString = "(" + left + " + " + right + ")"
}

case class Minus(left: Term, right: Term) extends Arithmetic {
  override def toString = "(" + left + " - " + right + ")"
}

case class Times(left: Term, right: Term) extends Arithmetic {
  override def toString = "(" + left + " * " + right + ")"
}

case class DividedBy(left: Term, right: Term) extends Arithmetic {
  override def toString = "(" + left + " / " + right + ")"
}

case class Transform(t: Term, f: Double => Double, name: String = "?") extends Term {
  def dim = t.dim
  override def toString = name + "(" + t + ")"
}

object InPorts {
  def apply(names: String*): Seq[InPort] = {
    for ((name, index) <- names.zipWithIndex)
      yield InPort(name, index)
  }
}

object OutPorts {
  def apply(names: String*): Seq[OutPort] = {
    for ((name, index) <- names.zipWithIndex)
      yield OutPort(name, index)
  }
}