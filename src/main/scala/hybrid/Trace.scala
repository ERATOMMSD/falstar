package hybrid

import linear.Vector

case class Trace(us: Signal, ys: Signal) {
  import Signal.SignalOps

  def isEmpty = ys.isEmpty

  def t0 = ys.t0
  def T = ys.T

  def until(t: Time) = {
    Trace(us until t, ys until t)
  }
}

object Trace {
  val empty = {
    Trace(Signal.empty, Signal.empty)
  }
}
