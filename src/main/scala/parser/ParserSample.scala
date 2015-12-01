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
//  def apply(input: String): Either[String, List[Nucleobase]] = parseAll(expr, input) match {
//    case Success(nucleobases, _) => Right(nucleobases)
//    case NoSuccess(msg, next)    => Left(s"$msg on line ${next.pos.line} on column ${next.pos.column}")
//  }

  def aa: Parser[Any] = "1" ~ "2" ~ "3" | "1" ~ "2" ~ "4" | "1" ~ "2" ~ "8"

  def bb: Parser[Any] = "3" ~ "77" ~ "5" | "1" ~ "2" ~ "8"

  def xx: Parser[Any] = "1" ~ "2" ~ "9" | "1" ~ "2" ~ "10"

  def cc: Parser[Any] = aa | bb

  def dd: Parser[Any] = aa | xx | bb

  def apply(input: String): Either[String, String] = parseAll(dd, input) match {
    case Success(nucleobases, _) => Right(nucleobases.toString)
    case Failure(msg, next) => Left(s"$msg on line ${next.pos.line} on column ${next.pos.column}")
  }


}


