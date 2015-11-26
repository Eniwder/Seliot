package parser

import org.scalatest.{FlatSpec, _}

import scala.util.parsing.combinator._
import scalaz._

/**
  * Created by slab on 2015/11/24.
  */
class ContextFreeSyntaxTest extends FlatSpec with Matchers with Parsers {

  import parser.ScalaParser.ContextFreeSyntax._
  import parser.ScalaParser.LexicalSyntax._

  "Literal Parser" should "IntegerLiteral parsed" in {
    shouldParse(Literal, "12345")
    shouldParse(Literal, "-12345L")
  }

  it should "BooleanLiteral parsed" in {
    shouldParse(Literal, "true")
    shouldParse(Literal, "false")
  }

  it should "FloatingLiteral parsed" in {
    shouldParse(Literal, "0.0")
    shouldParse(Literal, "123e30f")
    shouldParse(Literal, "-1.0e-100d")
  }

  it should "HexLiteral parsed" in {
    shouldParse(Literal, "0x123")
  }

  it should "StringLiteral parsed" in {
    shouldParse(Literal, "\"aiue\"")
    shouldParse(Literal, "\"\"\"aaa\"\"\"")
  }

  it should "SymbolLiteral parsed" in {
    shouldParse(symbolLiteral, "\'abc")
  }

  it should "CharacterLiteral parsed" in {
    shouldParse(Literal, "\'b\'")
  }

  it should "null Literal parsed" in {
    shouldParse(Literal, "null")
  }

  def shouldParse(expr: ScalaParser.Parser[Any], in: String) {
    val result = ScalaParser.run(expr, in)
    result match {
      case \/-(x) => info(x.toString)
      case -\/(msg) => info(msg)
    }
    result.isRight should be(true)
  }


}
