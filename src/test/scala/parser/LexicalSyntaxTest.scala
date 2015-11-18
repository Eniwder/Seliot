package parser

import org.scalatest.{FlatSpec, _}

import scalaz.syntax.either._

class LexicalSyntaxTest extends FlatSpec with Matchers {

  import parser.ScalaParser._
  import parser.ScalaParser.LexicalSyntax._

  "PrintableChar Parser" should "[e] parsed" in {
    ScalaParser.run(printableChar, "e") should be("e".right)
    info(ScalaParser.run(printableChar, "e").toString)
  }

}
