package falsification

import hybrid.Score
import hybrid.Signal
import hybrid.System
import hybrid.Time
import linear.Vector
import mtl.Formula
import mtl.Robustness
import hybrid.Rho
import hybrid.Input

object NelderMead {
  case class falsification(controlpoints: Int, budget: Int) extends Falsification with WithStatistics {
    override def productPrefix = "Falsification.neldermead"

    val identification = "NM"

    val params = Seq(
      "controlpoints" -> controlpoints,
      "budget" -> budget)

    def search(sys: System, phi: Formula, T: Time, sim: (Input, Signal, Time) => Result): Result = {
      val dt = T / controlpoints
      
      val i0 = sys.initial_region
      val in = sys.input_region

      val m = i0.dimensions
      val n = in.dimensions * controlpoints

      val x0 = for (d <- 0 to n) yield {
        val i = i0.sample
        val us = List.tabulate(controlpoints)(_ => in.sample)
        Vector((i :: us).flatten: _*)
      }

      def signal(z: Vector): (Input, Signal) = {
        val i = z(0 until m)
        val us = Signal(controlpoints, k => (dt * k, z(m + k * in.dimensions until m + (k + 1) * in.dimensions)))
        (i, us)
      }

      def feval(z: Vector): Score = {
        val (i, us) = signal(z)
        val res = sim(i, us, T)
        Falsification.observer.update(Seq(res))
        res.score
      }

      val fmin = 0
      val nmax = budget

      Falsification.observer.reset(phi)

      val lb = Vector(n, i => in.left(i % in.dimensions))
      val ub = Vector(n, i => in.right(i % in.dimensions))

      val (score, z) = util.NelderMead.minimize(feval, x0, lb, ub, fmin, nmax)

      val (i, us) = signal(z)
      sim(i, us, T)
    }
  }
}