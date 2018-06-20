package hybrid

import scala.io.StdIn

import com.mathworks.engine.MatlabEngine

import linear.Vector
import util.Timer
import java.io.OutputStreamWriter
import java.io.BufferedWriter

case class SimulinkSystem(
  path: String, name: String,
  initials: Seq[(String, (Double, Double))],
  inputs: Seq[(String, (Double, Double))],
  outputs: Seq[String],
  params: Seq[(String, String)] = Nil,
  vars: Seq[(String, String)] = Nil,
  load: Seq[String] = Nil)
  extends System {

  import Simulink._

  lazy val initialized = {
    object setup extends Timer

    setup.start()
    assert(engine != null)

    print("initializing '" + name + "' ...")

    eval("addpath('" + path + "')")
    eval("load_system('" + name + "')")

    if (!params.isEmpty) {
      eval("set_param('" + name + "'," + params.map { case (k, v) => k + "," + v }.mkString(", ") + ")")
    }

    for ((x, a) <- vars)
      eval(x + " = " + a)

    for (file <- load)
      eval("load('" + file + "')")

    setup.stop()
    println(" done (" + setup.seconds + "s)")

    true
  }

  def sim(i: Input, us: Signal, T: Time) = {
    assert(initialized)
    // println("simulate " + name + " from " + 0 + " to " + T)
    // println("simulate " + name + " to " + T + " with inputs " + us /*.collapse*/ .mkString(" "))

    for (k <- 0 until i.length)
      eval(initports(k).name + " = " + i(k))

    val t__ = us map { case (t, u) => Array(t) }
    val u__ = us map { case (t, u) => u.data }

    // NOTE: need to duplicate last entry in the input signal
    val U = u__.last
    eval("t__ = [" + t__.map(_.mkString(" ")).mkString("; ") + "; " + T + "]")
    eval("u__ = [" + u__.map(_.mkString(" ")).mkString("; ") + "; " + U.mkString(" ") + "]")

    // println("t__ = [" + t__.map(_.mkString(" ")).mkString("; ") + "; " + T + "]")
    // println("u__ = [" + u__.map(_.mkString(" ")).mkString("; ") + "; " + U.mkString(" ") + "]")

    eval("result = sim('" + name + "', 'StopTime', '" + T + "'" + ")")

    eval("tout = result.tout")
    eval("yout = result.yout.signals")
    eval("nout = size(yout, 2)")

    val ts: Array[Time] = get("tout")
    val nout: Double = get("nout")

    val yout = for (i <- 1 to nout.toInt) yield {
      val n = "y" + i
      eval(n + " = yout(" + i + ").values")
      val yi: Array[Double] = get(n)
      yi
    }

    val ys = yout.transpose

    val zs = Array.tabulate(ts.length) {
      i =>
        val t = ts(i)
        val y = Vector(ys(i): _*)
        (t, y)
    }

    assert(Math.abs(ts.last - T) < 0.1, "inconcistent simulink stopping time " + ts.last + " expected " + T)

    Trace(us, zs)
  }
}

object Simulink {
  var verbose = false
  var connected = false

  val stream = if (verbose)
    new BufferedWriter(new OutputStreamWriter(System.out))
  else
    MatlabEngine.NULL_WRITER

  val threshold = 0.0
  val separator = "-" * 20
  val nextResult = util.numbers(0)

  def main(args: Array[String]) {
    engine

    var run = true
    while (run) {
      try {
        val line = StdIn.readLine("> ")
        if (line.isEmpty) run = false
        else engine.eval(line)
      } catch {
        case _: Throwable =>
          run = false
      }
    }
    disconnect()
  }

  def eval(line: String) {
    val out = stream
    val err = stream
    if (verbose) println("matlab> " + line)
    engine.eval(line + ";", out, err)
  }

  def get[T](name: String): T = {
    engine.getVariable(name)
  }

  lazy val engine = {
    object timer extends Timer

    timer.start()
    print("starting matlab ...")

    val res = try {
      val res = MatlabEngine.connectMatlab()
      println(" connected (" + timer.seconds + "s)")
      res
    } catch {
      case _: Throwable =>
        val res = MatlabEngine.startMatlab()
        println(" done (" + timer.seconds + "s)")
        res
    }

    connected = true
    res
  }

  def disconnect() {
    if (connected)
      try { engine.disconnect() }
      finally {}
  }
}
