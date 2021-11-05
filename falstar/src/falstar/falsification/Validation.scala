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

  // calibrated via ForeSee
  val threshold = Map(
    "AT1" -> 1.2, // 1% of max speed
    "AT2" -> 10.0,  // close enough
    "AT6a" -> 1.0,
    "AT6b" -> 1.0,
    "AT6c" -> 1.0,
    "AT6abc" -> 1.0,
    "AFC27" -> 0.0008, // 10% of \beta
    "AFC29" -> 0.0007, // 10% of \gamma
    "AFC33" -> 0.0007, // 10% of \gamma
    "(AFC27 0.008)" -> 0.0008, // 10% of \beta
    "(AFC29 0.007)" -> 0.0007, // 10% of \gamma
    "(AFC33 0.007)" -> 0.0007, // 10% of \gamma
    "NNa" -> 0.1, // used by Breach as a synonym for "(NN 0.005 0.03)"
    "(NN 0.005 0.03)" -> 0.1, // somewhat large in comparison to the precision of the magnet
    "(NN 0.005 0.04)" -> 0.1,
    "NNx" -> 0.1,
    "CC1" -> 1.0,
    "CC2" -> 1.0,
    "CC3" -> 1.0,
    "CC4" -> 1.0,
    "CC5" -> 1.0,
    "CCx" -> 1.0,
    "SCa" -> 0.05, // 1% of valid range
  )

  val isTrue = Set("1", "true", "yes")

  def apply(table: Table, parser: Parser): (Seq[Row], Row) = {
    val rows = for(row <- table.rows; res <- apply(row, parser)) yield {
      print(".")
      res
    }

    val aggregate = ???

    (rows, aggregate)
  }

  def apply(row: Row, parser: Parser): List[Row] = {
    import Signal.TimeSeriesOps
    import Signal.SignalOps
    val data = row.data.toMap.asInstanceOf[Map[String,String]]

    try {
      val system = data("system")
      val state = parser.state
      val (sys, cfg) = state.systems(system)
      var validated: Option[Boolean] = None

      var property = data("property")

      // Hack for falsify:
      if(system == "NN" && property == "0.005-0.04")
        property = "(NN 0.005 0.04)"

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

      if(data contains "instance") {
          res += "instance" -> data("instance")
      }

      if(data contains "simulations") {
          res += "simulations" -> data("simulations")
      }

      res ++= state.notes

      if(data contains "input") {
          val ps = if(sys.params.isEmpty) Vector.empty
                   else Vector.parse(data.getOrElse("parameters", "[]"))
          
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
            validated = Some(false)
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
              val falsified = isTrue(expected)
              val falsified_ok = falsified == (rs.score < 0)
              res += "falsified correct" -> check(falsified_ok)

            if(falsified_ok) {
              if(falsified && validated.isEmpty)
                validated = Some(true)
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

                if(expected < 0 && validated.isEmpty && (threshold contains property)) {
                  println("considering threshold...")
                  validated = Some(computed < threshold(property))
                }
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
            println("output '" + n + "'")
            println("  min at time " + t1 + ": " + a1(i))
            println("  max at time " + t2 + ": " + a2(i))
          }

          try {
            if(data contains "output") {
              var ys = Signal.parse(data("output"))

              val ds = for((t, (x, y)) <- tr.ys synced ys) yield {
                (t, y - x)
              }

              val es = for((n, i) <- sys.outputs.zipWithIndex) yield {
                val (t, d) = ds.maxBy { case (t, x) => Math.abs(x(i)) }
                println("output '" + n + "'")
                println("  max discrepancy at time " + t + ": " + d(i))
                d(i)
              }

              res += "output error" -> (es maxBy Math.abs)
            }
          } catch {
            case e: Throwable =>
              println("output comparison failed: " + e.getMessage())
              println("outputs should be time series with the same format as the input")
              println("  - include the time as first component per entry")
              println("  - add all output fields")
          }

          if(validated == Some(true)) {
            println("VALIDATED")
            res += "validated" -> "true"
          } else {
            res += "validated" -> "false"
          }
            
          println()
          println()
      }

      List(Row(res))
    } catch {
      case e: Throwable =>
      if(data contains "property")
          println("error for " + data("property") + ": " + e)
      else if(data contains "system")
          println("error for " + data("system") + ": " + e)
      else
        println("error: " + e)
        Nil
    }
  }
}