package falstar.util

import java.io.BufferedWriter
import java.io.OutputStreamWriter

import scala.io.StdIn

import falstar.hybrid.Signal
import falstar.linear.Vector
import java.io.Writer
import java.lang.reflect.InvocationTargetException

object NullWriter extends Writer {
  def write(buf: Array[Char], off: Int, len: Int) {}
  def flush() {}
  def close() {}
}

object Matlab {
  var accelerated = false
  var verbose = false

  val klass = Class.forName("com.mathworks.engine.MatlabEngine")
  val connectMatlab0 = klass.getMethod("connectMatlab")
  val startMatlab0 = klass.getMethod("startMatlab")
  val disconnect0 = klass.getMethod("disconnect")
  val eval1 = klass.getMethod("eval", classOf[String])
  val eval3 = klass.getMethod("eval", classOf[String], classOf[Writer], classOf[Writer])
  val getVariable1 = klass.getMethod("getVariable", classOf[String])

  private var engine: Object = null
  
  val stream = if (verbose)
    new BufferedWriter(new OutputStreamWriter(System.out))
  else
    NullWriter

  val threshold = 0.0
  val separator = "-" * 20
  val nextResult = falstar.util.numbers(0)

  def main(args: Array[String]) {
    connect()
    var run = true

    while (run) {
      try {
        val line = StdIn.readLine("matlab> ")
        if (line == null) {
          run = false
        } else if (line == "quit") {
          run = false
        } else if (line(0) == '?') {
          val x = line.drop(1).trim
          println(x + " = " + getVariable1.invoke(engine, x))
        } else if (!line.isEmpty) {
          eval1.invoke(engine, line)
        }
      } catch {
        case e: InvocationTargetException =>
          println(e.getCause)
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
    assert(engine != null, "engine == null")
    assert(eval3 != null, "eval3 == null")
    try {
    eval3.invoke(engine, line + ";", out, err)
    } catch {
        case e: InvocationTargetException =>
          e.printCauseTrace()
          throw e
    }
  }

  def get[T](name: String): T = {
    getVariable1.invoke(engine, name).asInstanceOf[T]
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

  def connect() {
    object timer extends Timer
    if(engine == null) {
      timer.start()
      print("starting matlab ...")

      try {
        engine = connectMatlab0.invoke(null)
        println(" connected (" + timer.seconds + "s)")
      } catch {
        case _: Throwable =>
          engine = startMatlab0.invoke(null)
          println(" done (" + timer.seconds + "s)")
      }

      assert(engine != null, "matlab engine failed")
    }
  }

  def disconnect() {
    if(engine != null) {
      print("disconnecting matlab ...")
      disconnect0.invoke(engine)
      engine = null
    }
  }
}
