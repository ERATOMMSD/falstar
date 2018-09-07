package falstar.util

case class Row(data: Seq[(String, Any)]) {
  val (keys, values) = data.unzip
  assert(keys == keys.distinct)
}

case class Table(rows: Seq[Row]) {
  val keys = rows.flatMap(_.keys).distinct
}
