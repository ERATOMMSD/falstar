package falstar.util

import java.io.BufferedWriter
import java.io.OutputStreamWriter

import scala.io.StdIn

import com.mathworks.engine.MatlabEngine

import falstar.hybrid.Signal
import falstar.linear.Vector

object Simulink {
  var accelerated = false
  var verbose = false
  var connected = false

  val stream = if (verbose)
    new BufferedWriter(new OutputStreamWriter(System.out))
  else
    MatlabEngine.NULL_WRITER

  val threshold = 0.0
  val separator = "-" * 20
  val nextResult = falstar.util.numbers(0)

  def main(args: Array[String]) {
    engine

    var run = true
    while (run) {
      try {
        val line = StdIn.readLine("> ")
        if (line == null) {
          run = false
        } else if (line == "quit") {
          run = false
        } else if (line(0) == '?') {
          val x = line.drop(1).trim
          println(x + " = " + engine.getVariable(x))
        } else if (!line.isEmpty) {
          engine.eval(line)
        }
      } catch {
        case e: Throwable =>
          println(e)
        // run = false
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

  def row(name: String): Array[Double] = {
    get[Array[Double]](name)
  }

  // MATLAB returns single-column matrices [1;2;3] as 1-dimensional arrays
  // this causes type confusion when the model has a single output only
  def rows(name: String): Array[Array[Double]] = {
    get[Array[_]](name) match {
      case y: Array[Double] => y map (Array(_))
      case ys: Array[Array[Double]] => ys
    }
  }

  def signal(time: String, values: String): Signal = {
    val ts: Array[Double] = row(time)
    val um: Array[Array[Double]] = rows(values)
    val uv = um map (Vector(_: _*))
    val us = ts zip uv
    us
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
