package util

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import scalaz._
import scalaz.syntax.either._

trait ParserUtil extends RegexParsers{

  def run[P](expr: Parser[P], input: String): \/[String, _] = parseAll(expr, input) match {
    case Success(result, _) => result.right
    case Failure(msg, next) => s"$msg on line ${next.pos.line} on column ${next.pos.column}".left
  }

  def runp[P <: Positional](expr: Parser[P], input: String): \/[String, P] = parseAll(expr, input) match {
    case Success(result, _) => result.right
    case Failure(msg, next) => s"$msg on line ${next.pos.line} on column ${next.pos.column}".left
  }

}
