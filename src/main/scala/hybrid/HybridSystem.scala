package hybrid

import scala.collection.mutable.ArrayBuffer

import linear.Integrator
import linear.Vector

case class Mode(name: String, flow: Flow, switches: Seq[Switch]) {
}

object Mode {
  def default = "default"
}

case class Switch(from: String, to: String, enabled: Guard, jump: Transition) {
}

case class HybridSystem(
  name: String,
  start: (String, State),
  initials: Seq[(String, (Double, Double))],
  inputs: Seq[(String, (Double, Double))],
  outputs: Seq[String],
  readout: (String, State) => Vector,
  modes: Map[String, Mode],
  dt: Duration)

  extends System {

  val (m0, x0) = start

  def sim(us: Signal, T: Time): Trace = {
    sim(0, x0, m0, us, T)
  }

  def sim(t0: Time, x0: State, m0: String, us: Signal, T: Time): Trace = {
    val hmin = 0.00001
    val hmax = dt

    var t = t0
    var x = x0
    var m = modes(m0)
    var i = 0
    val n = us.length
    val xs = new ArrayBuffer[(Time, State)]()

    xs += ((t0, x0))

    while (i < n && t < T) {
      var go = true
      val (_, ui) = us(i)

      // XXX: all mode switches are urgent!
      while (go) {
        m.switches.find(_ enabled (t, x, ui)) match {
          case Some(s) =>
            // println("switch " + s.from + " -> " + s.to)
            x = s.jump(t, x, ui)
            // assert(!(s.from == s.to && s.enabled(t, x, ui)), "infinite switch")
            m = modes(s.to)
          case None =>
            go = false
        }
      }

      val (ti, _) = if (i + 1 < n) us(i + 1) else (T, us(n - 1))
      // val dt = ti - t

      // val tx = Integrator.rk4(flow, t, dt, x, ui)
      val tx = Integrator.dp45(m.flow, t, dt, x, ui, hmin, hmax,
        (t: Time, x: State) => xs += ((t, readout(m.name, x))))

      t = tx._1
      x = tx._2

      if (t >= ti) i += 1
      // xs += tx
    }

    val ys = xs.toArray[(Time, State)]
    Trace(us, ys)
  }

  def sim(tr: Trace, us: Signal, T: Time): Trace = {
    sim(tr.us ++ us, T)
  }
}