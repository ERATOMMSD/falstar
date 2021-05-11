package falstar

import java.io.File
import java.io.FileReader
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer
import java.io.Reader
import java.io.StringReader
import falstar.mtl.Port

package object parser {
  def parse(file: File) = {
    val syntax = read(file)
    val parser = new Parser
    val problems = parser.parse(syntax)

    problems
  }
  
  def formula(ports: Map[String, Port], phi: String) = {
    val tokens = scan(new StringReader(phi))
    val args = readAll(tokens)
    val node = Node(args: _*)
    val parser = new Parser
    parser.formula(ports: Map[String, Port], node: Syntax)
  }

  def read(file: File) = {
    val tokens = scan(new FileReader(file))
    val args = readAll(tokens)
    Node(args: _*)
  }

  
  def scan(reader: Reader) = {
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
