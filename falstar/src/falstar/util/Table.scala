package falstar.util

import java.io.File
import java.io.FileWriter
import java.io.FileReader
import java.io.Reader
import scala.collection.mutable.ArrayBuffer
import java.io.BufferedReader

import org.apache.commons.csv.CSVPrinter
import org.apache.commons.csv.CSVParser
import org.apache.commons.csv.CSVFormat

case class Row(data: Seq[(String, Any)]) {
  val (keys, values) = data.unzip
  assert(keys == keys.distinct)
}

case class Table(rows: Seq[Row]) {
  val columns = rows.flatMap(_.keys).distinct
  def ++(that: Table) = Table(this.rows ++ that.rows)
}

object Table {
  val empty = Table(Seq())

  def overwrite(table: Table, name: String) {
    import table.columns
    import table.rows

    val file = new File(name)

    val parent = file.getParentFile
    if (parent != null)
      parent.mkdirs()

    val writer = new FileWriter(file, false)
    val printer = new CSVPrinter(writer, CSVFormat.RFC4180)

    var first: Boolean = true
    printer.printRecord(columns: _*)

    for (row <- rows) {
      // a bit hacky
      val data = row.data.toMap.asInstanceOf[Map[String,AnyRef]]
      val entries = columns map { data.getOrElse(_, null) }
      printer.printRecord(entries: _*)
    }

    printer.flush()
    printer.close()
  }

  def write(table: Table, name: String, append: Boolean) {
    val file = new File(name)
    
    if(append && file.exists()) {
      val known = read(name)
      overwrite(known ++ table, name)
    } else {
      overwrite(table, name)
    }
  }

  def read(name: String): Table = {
    import scala.collection.JavaConverters._

    val reader = new FileReader(name)
    val parser = new CSVParser(reader, CSVFormat.RFC4180)

    val records = parser.iterator.asScala
    if(records.isEmpty) {
        Table.empty
    } else {
      val header = records.next()
      val columns = header.iterator.asScala.toList
      println(columns)
      val rows = for(record <- records) yield {
        val entries = record.iterator.asScala.toSeq
        Row(columns zip entries)
      }
      Table(rows.toSeq)
    }
  }

  def main(args: Array[String]) {
    val r1 = Row(Seq("x" -> 1, "y" -> 2))
    val r2 = Row(Seq("z" -> 3, "y" -> 2))
    val r3 = Row(Seq("z" -> 3, "x" -> 2))
    val t1 = Table(Seq(r1))
    val t2 = Table(Seq(r2))
    val t3 = Table(Seq(r3))
    Table.write(t1, "t.csv", true)
  }
}