import scala.language.implicitConversions
import mtl.Robustness
import mtl.Port
import mtl.OutPorts
import mtl.InPorts

package object hybrid {
  import linear._

  type Time = Double
  type Duration = Double

  type State = Vector
  type Input = Vector
  type Move = Vector

  type Score = Double

  type Guard = (Time, State, Input) => Boolean
  type Transition = (Time, State, Input) => State
  type Flow = (Time, State, Input) => Move

  type Signal = Array[(Time, Vector)]
  type Stop = Signal => Boolean

  type Rho = Trace => Robustness

  type Traces = Seq[Trace]
  
  object Transition {
    val skip = (t: Time, x: State, u: Input) => x
  }

  trait System {
    def name: String
    def inputs: Seq[(String, (Double, Double))]
    def outputs: Seq[String]

    val in = {
      val (left, right) = inputs.map(_._2).unzip
      Region(
        Vector(left: _*),
        Vector(right: _*))
    }

    val inports = InPorts(inputs map (_._1): _*)
    val outports = OutPorts(outputs: _*)

    def sim(us: Signal, T: Time): Trace
    def sim(tr: Trace, us: Signal, T: Time): Trace
  }

  object Score {
    val MinValue: Score = Double.NegativeInfinity
    val MaxValue: Score = Double.PositiveInfinity
  }

  object Time {
    val MinValue: Time = 0
    val MaxValue: Time = Double.PositiveInfinity
  }
}