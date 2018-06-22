import java.io.File
import java.io.FileWriter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.io.StdIn
import scala.util.control.Breaks

import falsification.Result
import falsification.Table
import hybrid.Simulink
import parser.Command
import parser.Falsify
import parser.Quit
import parser.Robustness
import parser.Simulate
import parser.parse
import util.Probability
import util.Scope
import parser.Flush

object Main {
  object quit extends Breaks

  object options {
    var a = false
    var v = false
    var g = false
    val sep = ","
  }

  val results = mutable.Map[String, mutable.Buffer[Table]]()

  def write(name: String, data: Seq[Table]) {
    /* val hostname = InetAddress.getLocalHost.getHostName
    val os = System.getProperty("os.name") + " " + System.getProperty("os.version")

    val date = new SimpleDateFormat("yyyyMMdd-HHmm").format(new Date)
    val file = new File(path + File.separator + date + "-" + hostname + ".csv") */

    val file = new File(name)
    import options.sep

    // ensure parent directory exists
    file.getParentFile.mkdirs()

    val falsifications = data.map(_.search)
    val writer = new FileWriter(file, true)

    val pre_cols = List("model", "property", "algorithm")
    val post_cols = List("seed", "success", "tries", "min simulations", "min time", "max simulations", "max time", "avg simulations", "avg time", "best robustness")
    val params_all = falsifications.flatMap(_.params)
    val (params_names, _) = params_all.unzip
    val extra_cols = params_names.distinct

    val cols = pre_cols ++ extra_cols ++ post_cols

    writer.write(cols.mkString(sep))
    writer.write("\n")

    for (table <- data) {
      val pmap = table.search.params.toMap

      writer.write(table.sys.name + sep)
      writer.write("\"" + table.phi + "\"" + sep)
      writer.write(table.search.identification + sep)
      for (col <- extra_cols) {
        if (pmap contains col)
          writer.write(pmap(col) + sep)
        else
          writer.write(sep)
      }
      writer.write(table.seed + sep)

      writer.write(table.success + sep)
      writer.write(table.tries + sep)
      writer.write(table.min.simulations + sep)
      writer.write(table.min.time + sep)
      writer.write(table.max.simulations + sep)
      writer.write(table.max.time + sep)
      writer.write(table.avg.simulations + sep)
      writer.write(table.avg.time + sep)
      writer.write(table.best.score + "\n")
    }

    writer.write("\n")
    writer.close()
  }

  def run(cmd: Command): Unit = cmd match {
    case Falsify(search, sys, phi, cfg, seed, repeat, log) =>
      seed match {
        case None => Probability.setUniqueSeed()
        case Some(seed) => Probability.seed = seed
      }

      val table = search.repeat(sys, phi, cfg, seed, repeat)
      val res @ Result(tr, rs) = table.best

      for (name <- log) {
        if (!(results contains name))
          results(name) = mutable.Buffer()
        results(name) += table
      }

      if (options.g) {
        val title = if (res.isFalsified) "falsified | " + sys.name + " | " + phi else "not falsified: " + phi
        val scope = new Scope(title, sys, res)
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

  def run(file: String): Unit = {
    val commands = parse(new File(file))

    quit.breakable {
      for (cmd <- commands) {
        run(cmd)
      }
    }
  }

  def writeall(results: Iterable[(String, mutable.Buffer[Table])]) {
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
      options.a = true
      setup(rest)
    case "-v" :: rest =>
      options.v = true
      setup(rest)
    case "-g" :: rest =>
      options.g = true
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

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("usage: falstar [-agv] file_1 ... file_n")
      println("  -a    ask for additional input files:")
      println("          enter one filename per line followed by a blank line")
      println("          a blank line acknowledges, EOF (CTRL+d) aborts")
      println("  -g    show a graphical diagram for each trial")
      println("  -v    be verbose")
    }

    val pre = List()
    val post = List("src/test/configuration/afc.cfg")
    val rest = setup(pre ++ args ++ post)

    var files = Buffer[String]()
    files ++= rest

    quit.breakable {
      while (options.a) {
        val line = StdIn.readLine

        if (line == null)
          quit.break
        else if (line.isEmpty)
          options.a = false
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