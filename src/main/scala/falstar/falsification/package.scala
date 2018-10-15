package falstar

import falstar.mtl.Formula

package object falsification {
  import falstar.hybrid.Score
  import falstar.hybrid.Time
  import falstar.hybrid.Trace
  import falstar.hybrid.System
  import falstar.mtl.Robustness
  import falstar.util.Timer

  type Results = Seq[Result]

  object Result {
    val empty = Result(Trace.empty, Robustness.empty)
  }

  case class Result(tr: Trace, rs: Robustness) {
    def T = tr.T

    def prefix(c: Score) = {
      val t = rs prefix (Score.MinValue, c)
      Result(tr until t, rs until t)
    }

    def score = rs.score
    def isFalsified = score < 0

    def time = (T, score)
    def robustness = (score, T)

    def until(t: Time) = {
      Result(tr until t, rs until t)
    }
  }

  // case class Table(sys: System, phi: Formula, search: Falsification, seed: Long, success: Int, tries: Int, min: Statistics, max: Statistics, avg: Statistics, best: Result)

  case class Statistics(simulations: Double, time: Double, memory: Double, score: Double)

  object Statistics {
    val empty = Statistics(0, 0, 0, 0)

    def _stdev(xs: Seq[Double]) = {
      val m = xs.sum / xs.length
      val ds = xs.map(x => (x - m) * (x - m))
      Math.sqrt(ds.sum / (xs.length - 1))
    }

    def zip(stats: Seq[Statistics], f: Seq[Double] => Double) = {
      if (stats.isEmpty) {
        Statistics.empty
      } else {
        val simulations = stats.map(_.simulations)
        val time = stats.map(_.time)
        val memory = stats.map(_.memory)
        val score = stats.map(_.score)
        Statistics(f(simulations), f(time), f(memory), f(score))
      }
    }

    def avg(stats: Seq[Statistics]) = zip(stats, xs => xs.sum / xs.length)
    def min(stats: Seq[Statistics]) = zip(stats, _.min)
    def max(stats: Seq[Statistics]) = zip(stats, _.max)
    def stdev(stats: Seq[Statistics]) = zip(stats, _stdev)
  }

  object Results {
    def feasible(stage: Results): Results = {
      ???
      // stage filter (_.isFeasible)
    }

    def filter(stage: Results, min: Score): Results = {
      stage filter (_.score <= min)
    }

    def filterStrict(stage: Results, min: Score): Results = {
      stage filter (_.score < min)
    }

    def partition(stage: Results, min: Score): (Results, Results) = {
      stage partition (_.score <= min)
    }

    def shortest(stage: Results, nkeep: Int): Results = {
      stage sortBy (_.time) take nkeep
    }

    def best(stage: Results, nkeep: Int): Results = {
      stage sortBy (_.robustness) take nkeep
    }
  }
}
