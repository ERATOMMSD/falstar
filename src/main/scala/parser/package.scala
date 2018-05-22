import java.io.File
import java.io.FileReader
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

package object parser {
  def parse(file: File) = {
    val syntax = read(file)
    val parser = new Parser
    val problems = parser.parse(syntax)

    problems
  }

  def read(file: File) = {
    val tokens = scan(file)
    val args = readAll(tokens)
    Node(args: _*)
  }

  def scan(file: File) = {
    val reader = new FileReader(file)
    val scanner = new Scanner(reader)
    val buffer = new ArrayBuffer[Token]()
    var token = scanner.next()
    while (token != null) {
      buffer += token
      token = scanner.next()
    }
    buffer.toSeq
  }

  def readAll(tokens: Seq[Token]): Seq[Syntax] = tokens match {
    case Seq() =>
      Seq()

    case _ =>
      val (arg, tail) = readOne(tokens)
      val args = readAll(tail)
      arg +: args
  }

  def readOne(tokens: Seq[Token]): (Syntax, Seq[Token]) = tokens match {
    case Seq(Keyword("("), tail @ _*) =>
      val (args, rest) = readList(tail)
      (Node(args: _*), rest)

    case Seq(tok: Syntax, rest @ _*) =>
      (tok, rest)
  }

  def readList(tokens: Seq[Token]): (Seq[Syntax], Seq[Token]) = tokens match {
    case Seq(Keyword(")"), rest @ _*) =>
      (Nil, rest)

    case _ =>
      val (arg, tail) = readOne(tokens)
      val (args, rest) = readList(tail)
      (arg +: args, rest)
  }
}
