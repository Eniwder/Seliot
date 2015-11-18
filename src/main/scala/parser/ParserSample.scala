package parser

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._


object ParserSample extends RegexParsers {

  sealed trait Nucleobase extends Positional
  case class G() extends Nucleobase
  case class A() extends Nucleobase
  case class T() extends Nucleobase
  case class C() extends Nucleobase

  def g = positioned("G" ^^ { case _ => G() })
  def a = positioned("AA" ^^ { case _ => A() })
  def t = positioned("T" ^^ { case _ => T() })
  def c = positioned("C" ^^ { case _ => C() })
  def expr = (g | a | t | c).*
  def apply(input: String): Either[String, List[Nucleobase]] = parseAll(expr, input) match {
    case Success(nucleobases, _) => Right(nucleobases)
    case NoSuccess(msg, next)    => Left(s"$msg on line ${next.pos.line} on column ${next.pos.column}")
  }
}

