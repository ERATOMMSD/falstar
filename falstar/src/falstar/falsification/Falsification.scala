package falstar.falsification

import falstar.hybrid.Config
import falstar.hybrid.Input
import falstar.hybrid.Signal
import falstar.hybrid.System
import falstar.hybrid.Time
import falstar.hybrid.Trace
import falstar.mtl.Formula
import falstar.mtl.Robustness
import falstar.util.Probability
import falstar.util.Row
import falstar.util.Timer
import falstar.util.now
import falstar.Main.Log

trait Falsification {
  def repeat(sys: System, cfg: Config, phi: Formula, _seed: Option[Long], n: Int, log: Log, notes: Seq[(String, Any)]): (Result, Seq[Signal], Seq[Row], Row) = {
    _seed match {
      case None => Probability.setUniqueSeed()
      case Some(seed) => Probability.seed = seed
    }

    val start = now()

    val data = (1 to n) map {
      i =>
        println("trial " + i + "/" + n)
        val (res, stat, row) = apply(sys, cfg, phi, log, notes)
        ((res, stat), row)
    }

    val end = now()

    val (all, rows) = data.unzip

    val (best, _) = all.minBy(_._1.score)
    val (good, bad) = all.partition(_._1.isFalsified)

    val (_, stats) = good.unzip
    // val table = Table(sys, phi, this, seed, good.size, n, Statistics.min(stats), Statistics.max(stats), Statistics.avg(stats), best)

    val stats_min = Statistics.min(stats)
    val stats_avg = Statistics.avg(stats)
    val stats_max = Statistics.max(stats)
    val stats_stdev = Statistics.stdev(stats)
    val stats_median = Statistics.median(stats)

    val what = Seq(
      "model" -> sys.name, "formula" -> phi,
      "start date" -> start,
      "end date" -> end
    )

    val how = Seq(
       "algorithm" -> this.identification,
    )

    val aggregate = Seq(
      "success" -> good.size, "trials" -> all.size,
      "min simulations" -> stats_min.simulations, "avg simulations" -> stats_avg.simulations, "median simulations" -> stats_median.simulations, "max simulations" -> stats_max.simulations, "stdev simulations" -> stats_stdev.simulations,
      "min time" -> stats_min.time, "avg time" -> stats_avg.time, "max time" -> stats_max.time, "stdev time" -> stats_stdev.time,
      "min robustness" -> stats_min.score, "avg robustness" -> stats_avg.score, "max robustness" -> stats_max.score, "stdev robustness" -> stats_stdev.score)
    /// "time" -> stats.time, "robustness" -> res.score

    (best, good map (_._1.tr.us), rows, Row(what ++ notes ++ how ++ params ++ aggregate))
  }

  def apply(sys: System, cfg: Config, phi: Formula, log: Log, notes: Seq[(String, Any)]): (Result, Statistics, Row) = {
    val seed = Probability.seed

    println("property " + phi)
    println("seed " + seed)

    println("algorithm " + identification)
    for ((name, value) <- this.params) {
      println("  " + name + ": " + value)
    }

    val start = now()
    val (res, stats) = search(sys, cfg, phi)
    val end = now()
    
    println()

    println("inputs")
    val Result(ps, Trace(us, ys), rs, ls) = res
    val T = phi.T

    import Signal.SignalOps

    if (!ps.isEmpty) {
      println("  p = " + ps.toMatlabRow)
    }

    if (us.isEmpty) {
      println("  u = [] (this should not happen!)")
    } else {
      println("  u = " + (us toMatlab T))
    }

    if (res.isFalsified) {
      print("falsified")
    } else {
      print("not falsified")
    }
    println(" with robustness " + rs.score)
    println()

    println("statistics")
    println("  simulations " + stats.simulations)
    println("  total time  " + stats.time + "s")
    println("  peak memory " + falstar.util.peakMemBytes / 1000 + " kb")
    println()

    val what = Seq(
      "model" -> sys.name, "formula" -> phi,
      "start date" -> start,
      "end date" -> end
    )

    val how = Seq(
       "algorithm" -> this.identification,
    )

    var data = Seq(
      "seed" -> seed, "simulations" -> stats.simulations, "time" -> stats.time, "robustness" -> res.score,
      "falsified" -> { if (res.isFalsified) "yes" else "no" },
      "parameters" -> { ps.toMatlabRow },
      "input" -> { if (!us.isEmpty) (us toMatlab T) else "[]" }
      )

    if(log.output)
      data ++= Seq(
        "output" -> { if (!ys.isEmpty) (ys toMatlab T) else "[]" }
      )

    if(log.robustness > 0) {
      var t: Time = 0
      
      data ++= ls.zipWithIndex map {
        case ((phi, rs), i) =>
          val rs_ = rs.downsample(log.robustness)
          ("robustness " + i + " of " + phi) -> (rs_.toMatlab)
      }
    }

    val row = Row(what ++ notes ++ how ++ params ++ data)

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
      import Signal.TimeSeriesOps

      val us_ = cfg.options get "sample" match {
        case _ if us.isEmpty => us
        case None => us sample (1, T)
        case Some(dt: Double) => us sample (dt, T)
      }

      val tr = simulation.during {
        sys.sim(ps, us_, T)
      }

      val (rs, log) = formula.during {
        Robustness.collect(phi, tr.us, tr.ys)
      }

      Result(ps, tr, rs, (phi -> rs) :: log)
    }

    val res = total.during {
      search(sys, cfg, phi, T, sim)
    }

    val stats = Statistics(simulations, total.seconds, 0, res.score)

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
