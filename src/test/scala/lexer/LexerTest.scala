package lexer

import org.scalatest._

class LexerTest extends FlatSpec with Matchers {
  val x = "xxx"

  x should be("xxx")
}
