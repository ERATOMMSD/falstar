package falstar

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.io.StdIn
import scala.util.control.Breaks

import falstar.hybrid.Signal
import falstar.parser.Command
import falstar.parser.Falsify
import falstar.parser.Flush
import falstar.parser.Quit
import falstar.parser.Robustness
import falstar.parser.Simulate
import falstar.parser.Validate
import falstar.parser.parse
import falstar.util.Probability
import falstar.util.Row
import falstar.util.Scope
import falstar.util.Matlab
import falstar.util.Table
import falstar.falsification.Validation

object Main {
  object quit extends Breaks

  class Log {
    var output = false
    var robustness = 0.0
  }

  class Options {
    var ask = false
    var verbose = false
    var graphics = false
    var dummy = false
    var append = true
    object log extends Log
    var args: List[String] = Nil
  }

  object options extends Options

  val results = mutable.Map[String, mutable.Buffer[Row]]()

  def write(name: String, data: Seq[Row]) {
    val table = Table(data)
    Table.write(table, name, options.append)
  }

  def run(cmd: Command): Unit = cmd match {
    case Falsify(search, sys, cfg, phi, seed, repeat, notes, log, report) =>
      seed match {
        case None => Probability.setUniqueSeed()
        case Some(seed) => Probability.seed = seed
      }

      val (best, good, rows, aggregate) = search.repeat(sys, cfg, phi, seed, repeat, options.log, notes)

      for (name <- log) {
        if (!(results contains name))
          results(name) = mutable.Buffer()
        results(name) ++= rows
      }

      for (name <- report) {
        if (!(results contains name))
          results(name) = mutable.Buffer()
        results(name) += aggregate
      }

      if (options.graphics) {
        val title = if (best.isFalsified) "falsified | " + sys.name + " | " + phi else "not falsified: " + phi
        val scope = new Scope(title, sys, best)
      }

    case Validate(log, report, parser) =>
      val table = Table.read(log.get)
      val rows = Validation.apply(table, parser)

      for (name <- report) {
        if (!(results contains name))
          results(name) = mutable.Buffer()
        results(name) ++= rows
      }

    case Simulate(sys, phi, ps, us, t) =>
      val tr = sys.sim(ps, us, t)
      val rs = mtl.Robustness(phi, tr.us, tr.ys)

    case Robustness(phi, us, ys, t) =>
      val rs = mtl.Robustness(phi, us, ys)

    case Flush =>
      writeall(results)
      results.clear()

    case Quit =>
      quit.break
  }

  def run(commands: Seq[Command]) {
    quit.breakable {
      for (cmd <- commands) {
        run(cmd)
      }
    }
  }

  def run(file: String): Unit = {
    val commands = parse(new File(file))

    if (options.dummy)
      return

    run(commands)
  }

  def writeall(results: Iterable[(String, mutable.Buffer[Row])]) {
    for ((name, data) <- results) {
      safe { write(name, data) }
    }
  }

  def runall(files: Iterable[String]) = {
    for (file <- files) {
      safe { run(file) }
    }
  }

  def setup(args: List[String]): List[String] = args match {
    case "-a" :: rest =>
      options.ask = true
      setup(rest)
    case "-v" :: rest =>
      Matlab.verbose = true
      options.verbose = true
      setup(rest)
    case "-g" :: rest =>
      options.graphics = true
      setup(rest)
    case "-d" :: rest =>
      options.dummy = true
      setup(rest)
    case "+r" :: sample :: rest =>
      options.log.robustness = sample.toDouble
      setup(rest)
    case "+o" :: rest =>
      options.log.output = true
      setup(rest)
    case "--" :: rest =>
      options.args = rest
      Nil
    case Nil =>
      Nil
    case file :: rest =>
      file :: setup(rest)
  }

  def safe(f: => Any) = {
    try { f }
    catch {
      case e: Throwable =>
        println("internal error: " + e)
        e.printStackTrace
    }
  }

  def robustness(_in: Array[String], _out: Array[String], _us: Array[Array[Double]], _ys: Array[Array[Double]], _phi: String): Double = {
    import falstar.linear.Vector

    val in = for ((name, index) <- _in.zipWithIndex)
      yield (name, mtl.InPort(name, index))
    val out = for ((name, index) <- _out.zipWithIndex)
      yield (name, mtl.OutPort(name, index))

    def cp(vs: Array[Double]) = {
      val t = vs.head
      val x = Vector(vs drop 1: _*)
      (t, x)
    }

    val phi = parser.formula((in ++ out).toMap, _phi)
    val us: Signal = _us map cp
    val ys: Signal = _ys map cp
    val rs = mtl.Robustness(phi, us, ys)
    rs.score
  }

  def shutdown() {
    writeall(results)
    println("bye")
    Matlab.disconnect
  }

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("usage: falstar [-dv] file_1 ... file_n")
      println("  -d    parse configuration only")
      println("  -v    be more verbose")
    }

    val files = setup(args.toList)

    object hook extends Thread {
      override def run() {
        shutdown()
      }
    }

    // make sure results get saved even on CTRL + C
    Runtime.getRuntime.addShutdownHook(hook)

    runall(files)
  }
}
