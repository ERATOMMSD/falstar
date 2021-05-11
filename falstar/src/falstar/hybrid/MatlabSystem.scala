package falstar.hybrid

import falstar.util.Timer

case class MatlabSystem(
  path: String, name: String,
  init: String, sim: String,
  params: Seq[String],
  inputs: Seq[String],
  outputs: Seq[String])
  extends System {

  import falstar.util.Matlab._

  lazy val initialized = {
    object setup extends Timer

    setup.start()
    connect()

    print("initializing '" + name + "' ...")

    eval("addpath('" + path + "')")
    eval(init)

    setup.stop()
    println(" done (" + setup.seconds + "s)")

    true
  }

  def sim(ps: Input, us: Signal, T: Time) = {
    import Signal.SignalOps

    for ((x, a) <- (params, ps.data).zipped)
      eval(x + " = " + a)

    assert(initialized)

    eval("u = " + (us toMatlab T))
    eval("result = " + sim + "(u, " + T + ")")
    eval("tout = result.tout")
    eval("yout = result.yout")
    val zs = signal("tout", "yout")

    assert(Math.abs(zs.T - T) < 0.1, "inconcistent simulink stopping time " + zs.T + " expected " + T)

    Trace(us, zs)
  }
}