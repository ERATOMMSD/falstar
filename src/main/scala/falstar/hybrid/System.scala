package falstar.hybrid

import falstar.linear.Vector
import falstar.mtl.InPorts
import falstar.mtl.OutPorts

sealed trait InputType {
  def range: (Double, Double)
}

case class Value(value: Double) extends InputType { def range = (value, value) }
case class Constant(min: Double, max: Double) extends InputType { def range = (min, max) }
case class PiecewiseConstant(min: Double, max: Double) extends InputType { def range = (min, max) }

case class Config(params: Map[String, InputType], inputs: Map[String, InputType], options: Map[String, Any]) {
  def region(names: Seq[String], data: Map[String, InputType]) = {
    val ranges = names map { name => data(name).range }
    val (left, right) = ranges.unzip
    Region(
      Vector(left: _*),
      Vector(right: _*))
  }

  def pn(names: Seq[String]) = region(names, params)
  def in(names: Seq[String]) = region(names, inputs)

  def cs(names: Seq[String]) = {
    for ((name, i) <- names.zipWithIndex if inputs(name).isInstanceOf[Constant]) yield i
  }
}

object Config {
  val empty = Config(Map(), Map(), Map())
}

trait System {
  def name: String
  def params: Seq[String]
  def inputs: Seq[String]
  def outputs: Seq[String]

  val inports = InPorts(inputs: _*)
  val outports = OutPorts(outputs: _*)

  def sim(ps: Input, us: Signal, T: Time): Trace
}
