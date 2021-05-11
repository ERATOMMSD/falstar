package falstar.hybrid

import falstar.util.Timer

case class SimulinkSystem(
  path: String, name: String,
  params: Seq[String],
  inputs: Seq[String],
  outputs: Seq[String],
  load: Seq[String] = Nil)
  extends System {

  import falstar.util.Matlab._

  lazy val initialized = {
    object setup extends Timer

    setup.start()
    connect()

    print("initializing '" + name + "' ...")

    eval("addpath('" + path + "')")
    eval("load_system('" + name + "')")

    // println("WARNING: set_param not implemented")
    // eval("set_param('" + name + "'," + params.map { case (k, v) => k + "," + v }.mkString(", ") + ")")

    for (file <- load) {
      if (file.endsWith(".m"))
        eval(file.drop(2))
      else if (file.endsWith(".mat"))
        eval("load('" + file + "')")
    }

    if (accelerated) {
      println(" compiling ")
      // eval("accelbuild('" + name + "')")
      eval("set_param('" + name + "','SimulationMode','rapid')")
    }

    setup.stop()
    println(" done (" + setup.seconds + "s)")

    true
  }

  def _sim(model: String, result: String, params: (String, Any)*) = {
    val args = params map { case (k, v) => ", '" + k + "', '" + v + "'" }
    eval(result + " = sim('" + name + "'" + args.mkString + ")")
  }

  def sim(ps: Input, us: Signal, T: Time) = {
    import Signal.SignalOps

    for ((x, a) <- (params, ps.data).zipped)
      eval(x + " = " + a)

    assert(initialized)

    eval("u = " + (us toMatlab T))

    _sim(name, "result",
      "StopTime" -> T,
      "LoadExternalInput" -> "on", "ExternalInput" -> "u",
      "SaveTime" -> "on", "TimeSaveName" -> "tout",
      "SaveOutput" -> "on", "OutputSaveName" -> "yout",
      "SaveFormat" -> "Array")

    eval("tout = result.tout")
    eval("yout = result.yout")
    val zs = signal("tout", "yout")

    assert(Math.abs(zs.T - T) < 0.1, "inconcistent simulink stopping time " + zs.T + " expected " + T)

    Trace(us, zs)
  }
}

