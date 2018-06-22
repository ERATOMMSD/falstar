package falsification

import hybrid.Config
import hybrid.Signal
import hybrid.System
import hybrid.Trace
import linear.Vector
import mtl.Always
import mtl.And
import mtl.Const
import mtl.DividedBy
import mtl.Equal
import mtl.Eventually
import mtl.False
import mtl.Formula
import mtl.Implies
import mtl.Less
import mtl.LessEqual
import mtl.Minus
import mtl.Not
import mtl.NotEqual
import mtl.Or
import mtl.Plus
import mtl.Port
import mtl.Robustness
import mtl.Term
import mtl.Times
import mtl.Transform
import mtl.True

object LaTeX {
  def print(tm: Term): String = tm match {
    case c: Const => c.toString
    case p: Port => p.name + "[t]"
    case Plus(left, right) => "(" + print(left) + " + " + print(right) + ")"
    case Minus(left, right) => "(" + print(left) + " - " + print(right) + ")"
    case Times(left, right) => "(" + print(left) + " * " + print(right) + ")"
    case DividedBy(left, right) => "(" + print(left) + " / " + print(right) + ")"
    case Transform(tm, _, f) if !f.isEmpty => f + "(" + print(tm) + ")"
  }

  def print(phi: Formula): String = phi match {
    case True => "true"
    case False => "false"

    case Less(left, right) => "(" + print(left) + " < " + print(right) + ")"
    case LessEqual(left, right) => "(" + print(left) + " \\le " + print(right) + ")"
    case Equal(left, right) => "(" + print(left) + " = " + print(right) + ")"
    case NotEqual(left, right) => "(" + print(left) + " \\neq " + print(right) + ")"

    case Not(phi) => "(\\lnot " + print(phi) + ")"
    case Or(phi, psi) => "(" + print(phi) + " \\lor " + print(psi) + ")"
    case And(phi, psi) => "(" + print(phi) + " \\land " + print(psi) + ")"
    case Implies(phi, psi) => "(" + print(phi) + " \\implies " + print(psi) + ")"

    case Always(t0, t1, phi) => "(\\Box_{[" + t0 + "," + t1 + "]} " + print(phi) + ")"
    case Eventually(t0, t1, phi) => "(\\Diamond_{[" + t0 + "," + t1 + "]} " + print(phi) + ")"
  }

  case object dummy extends Falsification {
    def identification = "LaTeX (print formulas only)"
    def params = Seq()

    def search(sys: System, cfg: Config,  phi: Formula): (Result, Statistics) = {
      println(print(phi))

      val us = Signal((0, Vector.zero(sys.inports.length)))
      val ys = Signal((0, Vector.zero(sys.outports.length)))
      val tr = Trace(us, Signal.empty)
      val rs = Robustness(Array((0.0, 0.0)))
      val res = Result(tr, rs)
      val stat = Statistics.empty
      (res, stat)
    }

  }

}