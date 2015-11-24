package parser

import org.scalatest.{FlatSpec, _}

import scalaz.syntax.either._

/**
  * Created by slab on 2015/11/24.
  */
class ContextFreeSyntaxTest extends FlatSpec with Matchers {

  import parser.ScalaParser.LexicalSyntax._

  "Literal Parser" should "IntegerLiteral parsed" in {
    ScalaParser.run(Literal, "12345") should be("12345".right)
    ScalaParser.run(Literal, "12345L") should be("12345L".right)
    info(ScalaParser.run(Literal, "12345").toString)
  }

  it should "BooleanLiteral parsed" in {
    ScalaParser.run(Literal, "true") should be("true".right)
    ScalaParser.run(Literal, "false") should be("false".right)
  }

  it should "FloatingLiteral parsed" in {
    ScalaParser.run(Literal, "0.0") should be("0.0".right)
    ScalaParser.run(Literal, "1e30f") should be("1e30f".right)
    ScalaParser.run(Literal, "1.0e-100") should be("1e30f".right)
    ScalaParser.run(Literal, "1.0e-.1") should be(".1".right)
  }

  it should "DecimalLiteral parsed" in {
    ScalaParser.run(Literal, "0123") should be("0123".right)
    ScalaParser.run(Literal, "0042") should be("0042".left)
    ScalaParser.run(Literal, "08") should be("08".left)
  }

  it should "HexLiteral parsed" in {
    ScalaParser.run(Literal, "0x123") should be("0x123".right)
    ScalaParser.run(Literal, "0X2BC") should be("0X2BC".left)
    ScalaParser.run(Literal, "0x12G") should be("0x12G".left)
  }

  it should "StringLiteral parsed" in {
    ScalaParser.run(Literal, "\"aiueo\"") should be("\"aiueo\"".right)
    ScalaParser.run(Literal, "\"\\\"aあ\"") should be("\"\\\"aあ\"".right)
  }

  it should "SymbolLiteral parsed" in {
    ScalaParser.run(Literal, "\'abc") should be("\'abc".right)
  }

  it should "CharacterLiteral parsed" in {
    ScalaParser.run(Literal, "\'b\'") should be("\'b\'".right)
  }

  it should "null Literal parsed" in {
    ScalaParser.run(Literal, "null") should be("null".right)
  }

}
