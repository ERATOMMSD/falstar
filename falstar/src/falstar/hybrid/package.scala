package falstar

import scala.language.implicitConversions
import falstar.mtl.Robustness
import falstar.mtl.Port
import falstar.mtl.OutPorts
import falstar.mtl.InPorts

package object hybrid {
  import falstar.linear._

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

  

  object Score {
    val MinValue: Score = Double.NegativeInfinity
    val MaxValue: Score = Double.PositiveInfinity
  }

  object Time {
    val MinValue: Time = 0
    val MaxValue: Time = Double.PositiveInfinity
  }
}
