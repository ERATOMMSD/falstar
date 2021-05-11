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

    if(data contains "input") {
        val ps = Vector.parse(data.getOrElse("parameters", "[]"))
        
        val us = if(data contains "times") {
          Signal.parse(data("times"), data("input"))
        } else {
          Signal.parse(data("input"))
        }
        // res += "input" -> input

        val pr = cfg.pn(sys.params)
        res += "parameters valid" -> check(pr contains ps)

        val ur = cfg.in(sys.inputs)
        res += "inputs valid" -> check(us forall { case (t, x) => ur contains x })

        val T = if(data contains "time horizon") {
            data("time horizon").toDouble
        } else {
            phi.T
        }

        val tr = sys.sim(ps, us, T)
        val rs = Robustness(phi, tr.us, tr.ys)

        if(data contains "falsified") {
            val expected = data("falsified")
            res += "falsified correct" -> check(isTrue(expected) == (rs.score < 0))
        }

        if(data contains "robustness") {
            val expected = data("robustness").toDouble
            val computed = rs.score
            val error = Math.abs(expected - computed)
            res += "robustness correct" -> check((expected < 0) == (computed < 0))
            res += "robustness error" -> error
        }

        if(data contains "output") {
          val ys = Signal.parse(data("output"))
        }
    }

    Row(res)
  }
}