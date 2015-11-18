package parser

import org.scalatest._
import parser.ScalaParser.UnicodeEscapes.UnicodeEscapeC
import parser.ScalaParser._

import scalaz.syntax.either._

class UnicodeEscapeTest extends FlatSpec with Matchers {

  val trg1 = "\\u12fA"
  val trg2 = "\\x120A"

  "Unicode Parser" should s"$trg1 parsed" in {
    ScalaParser.run(UnicodeEscapes.UnicodeEscape, trg1) should be(UnicodeEscapeC(trg1).right)
    ScalaParser.runp(UnicodeEscapes.UnicodeEscape, trg1).foreach(n => info(s"$n line: ${n.pos.line}, column: ${n.pos.column}"))
  }

  it should s"$trg2 doesn't parsed" in {
    ScalaParser.run(UnicodeEscapes.UnicodeEscape, trg2) should not be UnicodeEscapeC(trg2).right
  }


}
