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
import org.apache.commons.csv.QuoteMode
import scala.util.Try
import scala.reflect.ClassTag

case class Row(data: Seq[(String, Any)]) {
  val (keys, values) = data.unzip
  assert(keys == keys.distinct, "duplicate keys: " + keys.mkString(" "))

  def ++(that: Row) = Row(this.data ++ that.data)

  def get(key: String): Option[Any] = {
    data.find(_._1 == key) map (_._2)
  }

  def apply(key: String) = {
    val Some((_, value)) = data.find(_._1 == key)
    value
  }

  def apply(keys: Seq[String]): Seq[Any] = {
    val stuff = keys map get
    stuff.flatten
  }
}

case class Table(rows: Seq[Row]) {
  val columns = rows.flatMap(_.keys).distinct
  def ++(that: Table) = Table(this.rows ++ that.rows)

  def groupBy(index: Seq[String], aggregate: Seq[(String, String, Seq[Any] => Any)]): Table = {
    val grouped = rows.groupBy(_(index))
    import scala.math.Ordering.Implicits._
    val _rows = for((common, group) <- grouped.toSeq.sortBy(_._1.toString)) yield {
      val data = for((from, to, fun) <- aggregate) yield {
        val values = group flatMap (_ get from)
        (to, fun(values))
      }

      val prefix = index zip common
      Row(prefix ++ data)
    }

    Table(_rows.toSeq)
  }
}

object Table {
  val empty = Table(Seq())

  def dump(data: Seq[Seq[Any]], name: String) {
    val file = new File(name)

    val parent = file.getParentFile
    if (parent != null)
      parent.mkdirs()

    val format = CSVFormat.RFC4180.withQuoteMode(QuoteMode.NON_NUMERIC)
    val writer = new FileWriter(file, false)
    val printer = new CSVPrinter(writer, format)

    for(row <- data)
      printer.printRecord(row.asInstanceOf[Seq[AnyRef]]: _*)

    printer.close()
  }

  def overwrite(table: Table, name: String) {
    import table.columns
    import table.rows

    val file = new File(name)

    val parent = file.getParentFile
    if (parent != null)
      parent.mkdirs()

    val format = CSVFormat.RFC4180.withQuoteMode(QuoteMode.NON_NUMERIC)
    val writer = new FileWriter(file, false)
    val printer = new CSVPrinter(writer, format)

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
      val known = tryread(name)
      overwrite(known ++ table, name)
    } else {
      overwrite(table, name)
    }
  }

  def tryread(name: String): Table = {
    try { read(name) }
    catch { case e: Exception => Table.empty }
  }

  def read(name: String): Table = {
    import scala.collection.JavaConverters._

    val format = CSVFormat.DEFAULT
    val reader = new FileReader(name)
    val parser = new CSVParser(reader, format)

    val records = parser.iterator.asScala
    if(records.isEmpty) {
        Table.empty
    } else {
      val header = records.next()
      val columns = header.iterator.asScala.toList
      
      val rows = for(record <- records) yield {
        val entries = record.iterator.asScala.toList
        Row(columns zip entries)
      }
      Table(rows.toList)
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