package falstar.falsification

import falstar.hybrid.Config
import falstar.hybrid.Signal
import falstar.hybrid.System
import falstar.hybrid.Trace
import falstar.linear.Vector
import falstar.mtl.Always
import falstar.mtl.And
import falstar.mtl.Const
import falstar.mtl.DividedBy
import falstar.mtl.Equal
import falstar.mtl.Eventually
import falstar.mtl.False
import falstar.mtl.Formula
import falstar.mtl.Implies
import falstar.mtl.Less
import falstar.mtl.LessEqual
import falstar.mtl.Minus
import falstar.mtl.Not
import falstar.mtl.NotEqual
import falstar.mtl.Or
import falstar.mtl.Plus
import falstar.mtl.Port
import falstar.mtl.Robustness
import falstar.mtl.Term
import falstar.mtl.Times
import falstar.mtl.Transform
import falstar.mtl.True

object LaTeX {
  def print(tm: Term): String = tm match {
    case c: Const => c.toString
    case p: Port => "\\mathit{"+p.name + "}"
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
}