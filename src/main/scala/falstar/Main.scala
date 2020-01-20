package falstar

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.io.StdIn
import scala.util.control.Breaks

import falstar.parser.Command
import falstar.parser.Falsify
import falstar.parser.Flush
import falstar.parser.Quit
import falstar.parser.Robustness
import falstar.parser.Simulate
import falstar.parser.parse
import falstar.util.Probability
import falstar.util.Row
import falstar.util.Scope
import falstar.util.Simulink
import falstar.util.Table

object Main {
  object quit extends Breaks

  object options {
    var ask = false
    var verbose = false
    var graphics = false
    var dummy = false
    var append = false
    val sep = ','
  }

  val results = mutable.Map[String, mutable.Buffer[Row]]()

  def write(name: String, data: Seq[Row]) {
    val table = Table(data)
    table.write(name, options.sep, !options.append)
  }

  def run(cmd: Command): Unit = cmd match {
    case Falsify(search, sys, phi, cfg, seed, repeat, log, report) =>
      seed match {
        case None => Probability.setUniqueSeed()
        case Some(seed) => Probability.seed = seed
      }

      val (best, good, rows, aggregate) = search.repeat(sys, phi, cfg, seed, repeat)

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

  @tailrec
  def setup(args: List[String]): List[String] = args match {
    case "-a" :: rest =>
      options.ask = true
      setup(rest)
    case "-v" :: rest =>
      Simulink.verbose = true
      options.verbose = true
      setup(rest)
    case "-g" :: rest =>
      options.graphics = true
      setup(rest)
    case "-d" :: rest =>
      options.dummy = true
      setup(rest)
    case "+" :: rest =>
      options.append = true
      setup(rest)
    case _ =>
      args
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
    import falstar.hybrid.Signal
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

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("usage: falstar [-agv] [+] file_1 ... file_n")
      println("  -a    ask for additional input files:")
      println("          enter one filename per line followed by a blank line")
      println("          a blank line acknowledges, EOF (CTRL+d) aborts")
      println("  -d    dummy run, parse and validate configuration only")
      println("  -g    show a graphical diagram for each trial")
      println("  -v    be verbose")
      println("   +    no header in csv for next file (data should match previous header)")
    }

    val rest = setup(args.toList)

    var files = Buffer[String]()
    files ++= rest

    quit.breakable {
      while (options.ask) {
        val line = StdIn.readLine

        if (line == null)
          quit.break
        else if (line.isEmpty)
          options.ask = false
        else
          files += line
      }

      runall(files)
      writeall(results)
    }

    println("bye")

    Simulink.disconnect
  }
}
