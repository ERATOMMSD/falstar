import java.io.File
import java.net.InetAddress

import scala.annotation.tailrec
import scala.collection.mutable
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
import java.text.SimpleDateFormat
import java.io.FileWriter
import java.util.Date
import java.nio.file.Path

object Main {
  object quit extends Breaks

  object options {
    var v = false
    var g = false
  }

  val results = mutable.Map[String, mutable.Buffer[Table]]()

  def write(name: String, data: Seq[Table]) {
    /* val hostname = InetAddress.getLocalHost.getHostName
    val os = System.getProperty("os.name") + " " + System.getProperty("os.version")

    val date = new SimpleDateFormat("yyyyMMdd-HHmm").format(new Date)
    val file = new File(path + File.separator + date + "-" + hostname + ".csv") */

    val file = new File(name)

    // ensure parent directory exists
    file.getParentFile.mkdirs()

    val falsifications = data.map(_.search)
    val writer = new FileWriter(file, true)
    val params_all = falsifications.flatMap(_.params)
    val (params_names, _) = params_all.unzip
    val extra_cols = params_names.distinct

    writer.write("model;property;algorithm;" + extra_cols.mkString(";") + ";seed;success;tries;min simulations;min time;max simulations;max time;avg simulations;avg time;best robustness")
    writer.write("\n")

    for (table <- data) {
      val pmap = table.search.params.toMap

      writer.write(table.sys.name + ";")
      writer.write(table.phi + ";")
      writer.write(table.search.identification + ";")
      for (col <- extra_cols) {
        if (pmap contains col)
          writer.write(pmap(col) + ";")
        else
          writer.write(";")
      }
      writer.write(table.seed + ";")

      writer.write(table.success + ";")
      writer.write(table.tries + ";")
      writer.write(table.min.simulations + ";")
      writer.write(table.min.time + ";")
      writer.write(table.max.simulations + ";")
      writer.write(table.max.time + ";")
      writer.write(table.avg.simulations + ";")
      writer.write(table.avg.time + ";")
      writer.write(table.best.score + "\n")
    }
    
    writer.write("\n")
    writer.close()
  }

  def run(cmd: Command): Unit = cmd match {
    case Falsify(search, sys, phi, seed, repeat, log) =>
      seed match {
        case None => Probability.setUniqueSeed()
        case Some(seed) => Probability.seed = seed
      }

      val table = search.repeat(sys, phi, seed, repeat)
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

    case Simulate(sys, phi, us, t) =>
      val tr = sys.sim(us, t)
      val rs = mtl.Robustness(phi, tr.us, tr.ys)

    case Robustness(phi, us, ys, t) =>
      val rs = mtl.Robustness(phi, us, ys)

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

  @tailrec
  def setup(args: List[String]): List[String] = args match {
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
    if (args.isEmpty)
      println("usage: falstar [-r repeat] [-v] [-csv path] file_1 ... file_n")

    val rest = setup(args.toList)

    for (arg <- rest) {
      safe { run(arg) }
    }

    for ((name, data) <- results) {
      safe { write(name, data) }
    }

    println("bye")
    
    Simulink.disconnect
  }
}