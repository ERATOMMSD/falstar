package parser

sealed trait Syntax {
}

sealed trait Token extends Syntax {
}

case class Keyword(text: String) extends Token {
  override def toString = text
}

case class Identifier(text: String) extends Token {
  override def toString = text
}

case class Literal(value: Any) extends Token {
  override def toString = value match {
    case str: String => "\"" + str + "\""
    case _ => "" + value
  }
}

case class Node(args: Syntax*) extends Syntax {
  override def toString = {
    args.mkString("(", " ", ")")
  }
}