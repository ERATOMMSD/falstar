package falsification

import mtl.Robustness
import mtl.Formula
import hybrid.System
import hybrid.Region
import hybrid.Signal
import hybrid.Time
import hybrid.Rho

object UniformRandom {
  case class falsification(controlpoints: Int, budget: Int) extends Falsification with WithStatistics {
    override def productPrefix = "Falsification.random"

    def identification = "UR"

    val params = Seq(
      "controlpoints" -> controlpoints,
      "budget" -> budget)

    def search(sys: System, phi: Formula, T: Time, sim: (Signal, Time) => Result): Result = {
      val C = 0
      val dt = T / controlpoints

      print("falsification with " + budget + " random samples ")

      var best: Result = null

      Falsification.observer.reset(phi)

      for (k <- 1 to budget) {
        val us = Signal.uniform(0, dt, T)(sys.in.sample)
        val next = sim(us, T)
        print(".")

        Falsification.observer.update(next)

        if (best == null) best = next
        else if (next.score <= best.score) best = next
      }

      best
    }
  }
}