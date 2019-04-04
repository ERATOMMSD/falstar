package falstar.util

import java.io.File
import java.io.FileWriter
import java.io.FileReader
import java.io.Reader
import scala.collection.mutable.ArrayBuffer
import java.io.BufferedReader

case class Row(data: Seq[(String, Any)]) {
  val (keys, values) = data.unzip
  assert(keys == keys.distinct)
}

case class Table(rows: Seq[Row]) {
  val columns = rows.flatMap(_.keys).distinct

  def write(name: String, sep: Char, header: Boolean = true) {
    val file = new File(name)
    file.getParentFile.mkdirs()

    val writer = new FileWriter(file, true)

    var first: Boolean = true

    if (header) {
      for (column <- columns) {
        if (!first) writer.write(sep)
        writer.write(column)
        first = false
      }
      writer.write("\n")
    }

    for (row <- rows) {
      val map = row.data.toMap
      first = true
      for (column <- columns) {
        if (!first) writer.write(sep)
        if (map contains column) {
          val entry = map(column).toString
          val escape = (entry contains ' ') || (entry contains sep)
          if (escape) assert(!(entry contains Table.quote))
          if (escape) writer.write(Table.quote)
          writer.write(entry)
          if (escape) writer.write(Table.quote)
        }
        first = false
      }
      writer.write("\n")
    }

    // writer.write("\n")
    writer.flush()
    writer.close()
  }
}

class Bytes(reader: Reader) {
  var next = reader.read
  def atEof = next < 0
  def atNewline = next == '\n'

  def read: Int = {
    val x = next
    next = reader.read
    x
  }

  def test(x: Char) = {
    next == x
  }

  def expect(x: Char) = {
    assert(read == x)
  }

  override def toString = {
    next.asInstanceOf[Char].toString
  }
}

object Row {
  def escaped(reader: Bytes, sep: Char) = {
    val bytes = new ArrayBuffer[Byte]()
    while (!reader.atEof && !(reader test Table.quote)) {
      bytes append reader.read.asInstanceOf[Byte]
    }
    reader expect Table.quote
    new String(bytes.toArray)
  }

  def entry(reader: Bytes, sep: Char) = {
    val bytes = new ArrayBuffer[Byte]()
    while (!reader.atEof && !reader.atNewline && !reader.test(sep)) {
      bytes append reader.read.asInstanceOf[Byte]
    }
    new String(bytes.toArray)
  }

  def line(reader: Bytes, sep: Char): Seq[String] = {
    val entries = new ArrayBuffer[String]()
    while (!reader.atNewline) {
      if (reader test Table.quote)
        entries append escaped(reader, sep)
      else
        entries append entry(reader, sep)
    }
    entries
  }

  def read(reader: Bytes, columns: Seq[String], sep: Char): Row = {
    val data = line(reader, sep)
    Row(columns zip data)
  }

  def header(reader: Bytes, sep: Char): Seq[String] = {
    line(reader, sep)
  }
}

object Table {
  val quote = '"'

  def read(reader: Bytes, sep: Char): Table = {
    val rows = new ArrayBuffer[Row]()
    val columns = Row.header(reader, sep)

    while (!reader.atEof)
      rows append Row.read(reader, columns, sep)

    Table(rows)
  }

  def read(name: String, sep: Char): Table = {
    val file = new File(name)
    val reader = new Bytes(new FileReader(file))
    read(reader, sep)
  }

  def main(args: Array[String]) {
    println(Table.read("results/arch2018/summary.csv", ','))
  }
}
