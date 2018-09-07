package falstar.util

import java.io.File
import java.io.FileWriter

case class Row(data: Seq[(String, Any)]) {
  val (keys, values) = data.unzip
  assert(keys == keys.distinct)
}

case class Table(rows: Seq[Row]) {
  val columns = rows.flatMap(_.keys).distinct

  def write(name: String, sep: String) {
    val file = new File(name)
    file.getParentFile.mkdirs()

    val writer = new FileWriter(file, true)

    var first: Boolean = true
    for (column <- columns) {
      if (!first) writer.write(sep)
      writer.write(column)
      first = false
    }
    writer.write("\n")

    for (row <- rows) {
      val map = row.data.toMap
      first = true
      for (column <- columns) {
        if (!first) writer.write(sep)
        val entry = map(column).toString
        val escape = (entry contains ' ') || (entry contains sep)
        if (escape) writer.write("\"")
        writer.write(entry)
        if (escape) writer.write("\"")
        first = false
      }
      writer.write("\n")
    }

    writer.write("\n")
    writer.flush()
    writer.close()
  }
}
