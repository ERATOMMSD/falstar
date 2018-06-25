package falsification

import mtl.Robustness
import mtl.Formula
import hybrid.System
import hybrid.Region
import hybrid.Signal
import hybrid.Time
import hybrid.Rho
import hybrid.Config
import hybrid.Input

object UniformRandom {
  case class falsification(controlpoints: Int, budget: Int) extends Falsification with WithStatistics {
    override def productPrefix = "UniformRandom.falsification"

    def identification = "uniform random"

    val params = Seq(
      "control points" -> controlpoints,
      "budget" -> budget)

    def search(sys: System, cfg: Config, phi: Formula, T: Time, sim: (Input, Signal, Time) => Result): Result = {
      val C = 0
      val dt = T / controlpoints
      val pn = cfg.pn(sys.params)
      val in = cfg.in(sys.inputs)
      val cs = cfg.cs(sys.inputs)

      print("falsification with " + budget + " random samples ")

      var best: Result = null

      Falsification.observer.reset(phi)

      for (k <- 1 to budget) {
        val ps = pn.sample
        val u0 = in.sample

        val us = Signal(
          controlpoints,
          i => {
            val ui = in.sample
            for (c <- cs) ui(c) = u0(c)
            (dt * i, ui)
          })

        val next = sim(ps, us, T)
        print(".")

        Falsification.observer.update(next)

        if (best == null) best = next
        else if (next.score <= best.score) best = next

        if (best.score < 0) return best
      }

      best
    }
  }
}