package falstar.falsification

import scala.collection.mutable

import falstar.util.Row
import falstar.util.Table
import falstar.hybrid.Config
import falstar.mtl.Formula
import falstar.mtl.Robustness
import falstar.hybrid.System
import falstar.parser.Parser
import falstar.hybrid.Signal
import falstar.linear.Vector

object Validation {
  def check(ok: Boolean) = {
    if(ok) "yes" else "no"
  }

  val isTrue = Set("1", "true", "yes")

  def apply(table: Table, parser: Parser): Seq[Row] = {
    for(row <- table.rows) yield {
      val res = apply(row, parser)
      print(".")
      res
    }
  }

  def apply(row: Row, parser: Parser): Row = {
    import Signal.TimeSeriesOps
    import Signal.SignalOps

    val data = row.data.toMap.asInstanceOf[Map[String,String]]

    val system = data("system")
    val state = parser.state
    val (sys, cfg) = state.systems(system)

    val property = data("property")
    // need to unpack one layer because it reads a sequence of nodes
    val falstar.parser.Node(node) = falstar.parser.read(property)
    state.system = sys // such that ports and stuff work
    val phi = parser.formula(node)

    val res = mutable.Buffer[(String, Any)]()

    res += "system" -> system
    res += "property" -> property
    res += "formula" -> phi

    println("system: " + system)
    println("property: " + property)
    println("known formula: " + phi)

    if(data contains "formula") {
      println("reference formula: " + data("formula"))
    }

    if(data contains "input") {
        val ps = Vector.parse(data.getOrElse("parameters", "[]"))
        
        var us = if(data contains "times") {
          Signal.parse(data("times"), data("input"))
        } else {
          Signal.parse(data("input"))
        }
        // res += "input" -> input

        val pr = cfg.pn(sys.params)
        res += "parameters valid" -> check(pr contains ps)

        val ur = cfg.in(sys.inputs)

        val us_ok = us forall { case (t, x) => ur contains x }
        res += "inputs valid" -> check(us_ok)

        if(!us_ok) {
          val (t, x) = us maxBy { case (t, x) => ur error x }
          val error = ur error x
          res += "inputs error" -> error
          res += "inputs invalid where" -> ("[" + t + " " + x.data.mkString(" ") + "]")
        }

        if(us_ok) {
          println("input is within bounds")
        } else {
          println("input is NOT within bounds")
        }

        val dt = us.dt
        val isUpsampled = (dt < 1.0)

        val T = if(data contains "stop time") {
          print("using stop time as provided: ")
          data("stop time").toDouble
        } else if(us.length <= 1 || !isUpsampled) {
          print("using stop time from formula: ")
          phi.T
        } else {
          print("using stop time from input signal: ")
          us.T
        }

        println(T)

        if(!isUpsampled) {
          println("upsampling input signal with dt = " + 0.1)
          us = us.sample(0.1, T)
        }

        val tr = sys.sim(ps, us, T)
        val (rs, sub) = Robustness.collect(phi, tr.us, tr.ys)

        if(data contains "falsified") {
            val expected = data("falsified")
            val falsified_ok = isTrue(expected) == (rs.score < 0)
            res += "falsified correct" -> check(falsified_ok)

          if(falsified_ok) {
            println("falsified is correct")
          } else {
            println("falsified is incorrect")
          }
        }

        if(data contains "robustness") {
            val expected = data("robustness").toDouble
            val computed = rs.score

            // work around for infinities
            val error = if(expected == computed) 0.0
              else Math.abs(expected - computed)

            val robustness_ok = (expected < 0) == (computed < 0)
            res += "robustness expected" -> expected
            res += "robustness computed" -> computed
            res += "robustness correct" -> check(robustness_ok)
            res += "robustness error" -> error
            
            println("expected robustness " + expected)
            println("computed robustness " + computed)
            println("robustness error " + error)

            if(!robustness_ok) {
              println("robustness sign is incorrect!")
            }
        } else {
            val computed = rs.score
            res += "robustness computed" -> computed
            println("robustness computed " + computed)
        }

        println("robustness of subformulas (at time T = " + T + ")")
        val sub_ = sub.map { case (phi, rs) => (phi, rs.score) }
        for((phi, score) <- sub_.distinct) {
          println(String.format("%8.2f", score: java.lang.Double) + "  " + phi)
        }

        for((n, i) <- sys.outputs.zipWithIndex) {
          val (t1, a1) = tr.ys.minBy { case (t, x) => x(i) }
          val (t2, a2) = tr.ys.maxBy { case (t, x) => x(i) }
          println("input '" + n + "'")
          println("  min at time " + t1 + ": " + a1(i))
          println("  max at time " + t2 + ": " + a2(i))
        }

        if(data contains "output") {
          val ys = Signal.parse(data("output"))
        }

        println()
        println()
    }

    Row(res)
  }
}