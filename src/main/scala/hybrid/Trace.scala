package hybrid

import linear.Vector

case class Trace(i: Input, us: Signal, ys: Signal) {
  import Signal.SignalOps

  def isEmpty = ys.isEmpty

  def t0 = ys.t0
  def T = ys.T

  def until(t: Time) = {
    Trace(i, us until t, ys until t)
  }
}

object Trace {
  val empty = {
    Trace(Vector.empty, Signal.empty, Signal.empty)
  }
}