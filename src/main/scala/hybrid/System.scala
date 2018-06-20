package hybrid
import linear.Vector

sealed trait InputType
case class ConstantValue(value: Double) extends InputType
case class Constant(min: Double, max: Double) extends InputType
case class PiecewiseConstant(min: Double, max: Double) extends InputType

case class Configuration(params: Map[String, InputType], inputs: Map[String, InputType]) {
  val initial_region = {
    val (left, right) = params.map(_._2).unzip

    Region(
      Vector(left: _*),
      Vector(right: _*))
  }

  val input_region = {
    val (left, right) = inputs.map(_._2).unzip
    Region(
      Vector(left: _*),
      Vector(right: _*))
  }
}

trait System {
  def name: String
  def params: Seq[String]
  def inputs: Seq[String]
  def outputs: Seq[String]
  def sim(i: Input, us: Signal, T: Time): Trace
}
