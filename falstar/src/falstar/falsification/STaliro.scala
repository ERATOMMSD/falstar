package falstar.falsification

import falstar.hybrid.Signal
import falstar.hybrid.System
import falstar.hybrid.Trace
import falstar.linear.Vector
import falstar.mtl.Always
import falstar.mtl.And
import falstar.mtl.Equal
import falstar.mtl.Eventually
import falstar.mtl.False
import falstar.mtl.Formula
import falstar.mtl.Less
import falstar.mtl.LessEqual
import falstar.mtl.NotEqual
import falstar.mtl.Or
import falstar.mtl.Robustness
import falstar.mtl.Term
import falstar.mtl.True
import falstar.mtl.Not
import falstar.mtl.Implies
import falstar.mtl.Proposition
import falstar.hybrid.Config

object STaliro {
  case class dummy(prefix: String) extends Falsification {
    def identification = "S-Taliro (print formulas only)"
    def params = Seq()

    var pi = 0
    var fi = 0
    var props = Map[Proposition, Int]()

    def print(prop: Proposition): String = {
      if (!(props contains prop)) {
        props += (prop -> pi)
        println("props(" + pi + ").str = '" + prefix + pi + "'; % " + prop)
        pi += 1
      }

      val i = props(prop)
      val name = prefix + i
      name
    }

    def print(phi: Formula): String = phi match {
      case True => "true"
      case False => "false"
      case prop: Proposition => print(prop)
      case Not(phi) => "(! " + print(phi) + ")"
      case Or(phi, psi) => "(" + print(phi) + " \\/ " + print(psi) + ")"
      case And(phi, psi) => "(" + print(phi) + " /\\ " + print(psi) + ")"
      case Implies(phi, psi) => "(" + print(phi) + " -> " + print(psi) + ")"
      case Always(t0, t1, phi) => "([]_[" + t0 + "," + t1 + "] " + print(phi) + ")"
      case Eventually(t0, t1, phi) => "(<>_[" + t0 + "," + t1 + "] " + print(phi) + ")"
    }

    def search(sys: System, cfg: Config, phi: Formula): (Result, Statistics) = {
      println(sys.name + "_phi" + fi + "' = " + print(phi) + "';")
      fi += 1

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