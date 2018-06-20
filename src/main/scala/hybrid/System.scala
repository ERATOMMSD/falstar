package hybrid

import linear.Vector
import mtl.InPorts
import mtl.OutPorts

sealed trait InputType {
  def range: (Double, Double)
}

case class Value(value: Double) extends InputType { def range = (value, value) }
case class Constant(min: Double, max: Double) extends InputType { def range = (min, max) }
case class PiecewiseConstant(min: Double, max: Double) extends InputType { def range = (min, max) }

case class Config(params: Map[String, InputType], inputs: Map[String, InputType], options: Map[String, Any]) {
  val in = {
    val (left, right) = inputs.map(_._2.range).toSeq.unzip
    Region(
      Vector(left: _*),
      Vector(right: _*))
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

  def sim(us: Signal, T: Time): Trace
}