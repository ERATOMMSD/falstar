package falstar.falsification

import falstar.mtl.Robustness
import falstar.mtl.Formula
import falstar.hybrid.System
import falstar.hybrid.Region
import falstar.hybrid.Signal
import falstar.hybrid.Time
import falstar.hybrid.Rho
import falstar.hybrid.Config
import falstar.hybrid.Input
import falstar.linear.Vector
import falstar.util.NelderMead


object Optimization {
  case class falsification(budget: Int) extends Falsification with WithStatistics {
    override def productPrefix = "Optimization.falsification"

    def identification = "nelder mead"

    val params = Seq(
      "budget" -> budget)

    def search(sys: System, cfg: Config, phi: Formula, T: Time, sim: (Input, Signal, Time) => Result): Result = {
      val pn = cfg.pn(sys.params)
      val in = cfg.in(sys.inputs)

      assert(sys.inputs.isEmpty, "unsupported: multiple input dimensions")

      println("falsification with " + budget + " nelder mead samples")
      println("parameters: " + sys.params.mkString(", "))

      Falsification.observer.reset(phi)
      val us = Signal((0, Vector.empty))

      def feval(ps: Vector) = {
        val rs = sim(ps, us, T)
        (rs, rs.score)
      }

      val p0 = pn.corners
      println("initial samples: " + p0)
      val (rs, _, _) = NelderMead.minimize(feval, p0, pn.left, pn.right, 0.0, budget)
      rs
    }
  }
}