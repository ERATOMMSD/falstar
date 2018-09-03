package falstar.mtl

import falstar.hybrid.Score
import falstar.hybrid.Time
import falstar.linear.Vector

sealed trait Formula {
  def T: Time
  def dim: Int
  def times: Seq[Time]

  def unary_! : Formula = this match {
    case True => False
    case False => True
    case _ => Not(this)
  }

  def &&(that: Formula): Formula = (this, that) match {
    case (True, _) => that
    case (_, True) => this
    case (False, _) => False
    case (_, False) => False
    case _ => And(this, that)
  }

  def ||(that: Formula): Formula = (this, that) match {
    case (True, _) => True
    case (_, True) => True
    case (False, _) => that
    case (_, False) => this
    case _ => Or(this, that)
  }

  def ==>(that: Formula): Formula = (this, that) match {
    case (False, _) => True
    case (_, True) => True
    case (True, _) => that
    case (_, False) => !this
    case _ => Implies(this, that)
  }
}

sealed trait Proposition extends Formula {
  def T = 0
  def times = Seq()
}

sealed trait Constant extends Proposition {
  def dim = 0
}

sealed trait Constraint extends Proposition {
  def dim = Math.max(left.dim, right.dim)
  def left: Term
  def right: Term
}

object Constraint {
  var threshold: Score = 1e-4
}

sealed trait Connective extends Formula {
  def left: Formula
  def right: Formula
  def T = Math.max(left.T, right.T)
  def dim = Math.max(left.dim, right.dim)
  def times = left.times ++ right.times
}

sealed trait Modality extends Formula {
  def from: Time
  def to: Time
  def phi: Formula
  def dim = phi.dim
  def times = Seq(from, to)
}

case object True extends Constant
case object False extends Constant

case class Less(left: Term, right: Term) extends Constraint {
  override def toString = "(" + left + " < " + right + ")"
}

case class LessEqual(left: Term, right: Term) extends Constraint {
  override def toString = "(" + left + " <= " + right + ")"
}

case class Equal(left: Term, right: Term) extends Constraint {
  override def toString = "(" + left + " == " + right + ")"
}

case class NotEqual(left: Term, right: Term) extends Constraint {
  override def toString = "(" + left + " != " + right + ")"
}

case class Not(phi: Formula) extends Formula {
  def T = phi.T
  def dim = phi.dim
  def times = phi.times
  override def toString = "!" + phi
}

case class And(left: Formula, right: Formula) extends Connective {
  override def toString = "(" + left + " && " + right + ")"
}

case class Or(left: Formula, right: Formula) extends Connective {
  override def toString = "(" + left + " || " + right + ")"
}

case class Implies(left: Formula, right: Formula) extends Connective {
  override def toString = "(" + left + " ==> " + right + ")"
}

case class Always(from: Time, to: Time, phi: Formula) extends Modality {
  assert(0 <= from && from <= to && to < Double.PositiveInfinity)
  def T = to
  override def toString = "□_[" + from + ", " + to + "] " + phi
}

case class Eventually(from: Time, to: Time, phi: Formula) extends Modality {
  assert(0 <= from && from <= to && to < Double.PositiveInfinity)
  def T = to
  override def toString = "◇_[" + from + ", " + to + "] " + phi
}
  
  