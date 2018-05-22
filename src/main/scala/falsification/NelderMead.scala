package falsification

import hybrid.Score
import hybrid.Signal
import hybrid.System
import hybrid.Time
import linear.Vector
import mtl.Formula
import mtl.Robustness
import hybrid.Rho

object NelderMead {
  case class falsification(controlpoints: Int, budget: Int) extends Falsification with WithStatistics {
    override def productPrefix = "Falsification.neldermead"

    val identification = "NM"

    val params = Seq(
      "controlpoints" -> controlpoints,
      "budget" -> budget)

    def search(sys: System, phi: Formula, T: Time, sim: (Signal, Time) => Result): Result = {
      val dt = T / controlpoints
      val in = sys.in

      val n = in.dimensions * controlpoints

      val x0 = for (d <- 0 to n) yield {
        val us = Seq.tabulate(controlpoints)(_ => in.sample)
        Vector(us.flatten: _*)
      }

      def signal(z: Vector): Signal = {
        Signal(controlpoints, i => (dt * i, z(i * in.dimensions until (i + 1) * in.dimensions)))
      }

      def feval(z: Vector): Score = {
        val us = signal(z)
        val res = sim(us, T)
        Falsification.observer.update(Seq(res))
        res.score
      }

      val fmin = 0
      val nmax = budget

      Falsification.observer.reset(phi)

      val lb = Vector(n, i => in.left(i % in.dimensions))
      val ub = Vector(n, i => in.right(i % in.dimensions))

      val (score, z) = util.NelderMead.minimize(feval, x0, lb, ub, fmin, nmax)

      val us = signal(z)
      sim(us, T)
    }
  }
}