package falstar.falsification

import falstar.hybrid.Config
import falstar.hybrid.Input
import falstar.hybrid.Signal
import falstar.hybrid.System
import falstar.hybrid.Time
import falstar.mtl.Formula
import falstar.mtl.Robustness
import falstar.util.Probability
import falstar.util.Row
import falstar.util.Timer

trait Falsification {
  def repeat(sys: System, cfg: Config, phi: Formula, _seed: Option[Long], n: Int): (Result, Seq[Row]) = {
    _seed match {
      case None => Probability.setUniqueSeed()
      case Some(seed) => Probability.seed = seed
    }

    val data = (1 to n) map {
      i =>
        println("trial " + i + "/" + n)
        val (res, stat, row) = apply(sys, cfg, phi)
        ((res, stat), row)
    }

    val (all, rows) = data.unzip

    val (best, _) = all.minBy(_._1.score)
    val (good, bad) = all.partition(_._1.isFalsified)

    val (_, stats) = good.unzip
    // val table = Table(sys, phi, this, seed, good.size, n, Statistics.min(stats), Statistics.max(stats), Statistics.avg(stats), best)

    (best, rows)
  }

  def apply(sys: System, cfg: Config, phi: Formula): (Result, Statistics, Row) = {
    val seed = Probability.seed

    println("property " + phi)
    println("seed " + seed)
    
    println("algorithm " + identification)
    for ((name, value) <- this.params) {
      println("  " + name + ": " + value)
    }

    val (res, stats) = search(sys, cfg, phi)
    println()

    println("inputs")
    val us = res.tr.us
    val t__ = us map { case (t, u) => Array(t) }
    val u__ = us map { case (t, u) => u.data }
    val U = u__.last
    val T = res.T
    println("  t__ = [" + t__.map(_.mkString(" ")).mkString("; ") + "; " + T + "]")
    println("  u__ = [" + u__.map(_.mkString(" ")).mkString("; ") + "; " + U.mkString(" ") + "]")

    if (res.isFalsified) {
      print("falsified")
    } else {
      print("not falsified")
    }
    println(" with robustness " + res.score)
    println()

    println("statistics")
    println("  simulations " + stats.simulations)
    println("  total time  " + stats.time + "s")
    println("  peak memory " + falstar.util.peakMemBytes / 1000 + " kb")
    println()

    val data = Seq(
      "model" -> sys.name, "property" -> phi, "algorithm" -> this.identification,
      "seed" -> seed, "simulations" -> stats.simulations, "time" -> stats.time, "robustness" -> res.score,
      "falsified" -> { if (res.isFalsified) "yes" else "no" })

    val row = Row(data ++ params)

    // expose another seed for the next trial
    // required for external algorithms (Breach, S-Taliro)
    // nice for internal algorithms because the new seed shows up in the report
    Probability.setNextDeterministicSeed()

    (res, stats, row)
  }

  def identification: String
  def params: Seq[(String, Any)]
  def search(sys: System, cfg: Config, phi: Formula): (Result, Statistics)
}

trait WithStatistics {
  this: Falsification =>

  def search(sys: System, cfg: Config, phi: Formula): (Result, Statistics) = {
    var simulations = 0
    object simulation extends Timer
    object formula extends Timer
    object total extends Timer

    val T = phi.T

    def sim(ps: Input, us: Signal, T: Time): Result = {
      simulations += 1
      val tr = simulation.during {
        sys.sim(ps, us, T)
      }
      val rs = formula.during {
        Robustness(phi, tr.us, tr.ys)
      }
      Result(tr, rs)
    }

    val res = total.during {
      search(sys, cfg, phi, T, sim)
    }

    val stats = Statistics(simulations, total.seconds, 0)

    (res, stats)
  }

  def search(sys: System, cfg: Config, phi: Formula, T: Time, sim: (Input, Signal, Time) => Result): Result
}

object Falsification {
  trait Observer {
    def reset(phi: Formula)
    def update(tr: Result) { update(Seq(tr)) }
    def update(trs: Results)
  }

  object Observer {
    object default extends Observer {
      def reset(phi: Formula) {}
      def update(trs: Results) {}
    }
  }

  var observer: Observer = Observer.default
}