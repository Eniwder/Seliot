package parser

import util.ParserUtil

import scala.util.parsing.input.Positional

object ScalaParser extends ParserUtil {

  object UnicodeEscapes {

    case class UnicodeEscapeC(str: String) extends Positional

    lazy val UnicodeEscape = positioned(
      ("\\" ~ "u" ~ rep("u") ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
        ^^ { case bs ~ u ~ ul ~ h1 ~ h2 ~ h3 ~ h4 => UnicodeEscapeC("" + bs + u + ul.mkString("") + h1 + h2 + h3 + h4) }
    )

    lazy val hexDigit = """[0-9|a-f|A-F]""".r

  }

  object LexicalSyntax {

    // whiteSpace       ::=  ‘\\u0020’ | ‘\\u0009’ | ‘\\u000D’ | ‘\\u000A’
    // override lazy val whiteSpace = "\\u0020" | "\\u0009" | "\\u000D" | "\\u000A"
    // upper            ::=  ‘A’ | … | ‘Z’ | ‘$’ | ‘_’  // and Unicode category Lu
    lazy val upper =
      """[A-Z]""".r | "$" | "_"
    // lower            ::=  ‘a’ | … | ‘z’ // and Unicode category Ll
    lazy val lower =
      """[a-z]""".r
    // letter           ::=  upper | lower // and Unicode categories Lo, Lt, Nl
    lazy val letter = upper | lower
    // digit            ::=  ‘0’ | … | ‘9’
    lazy val digit =
      """[0-9]""".r
    // paren            ::=  ‘(’ | ‘)’ | ‘[’ | ‘]’ | ‘{’ | ‘}’
    lazy val paren = "(" | ")" | "[" | "]" | "" | ".*"
    // delim            ::=  ‘`’ | ‘'’ | ‘"’ | ‘.’ | ‘;’ | ‘,’
    lazy val delim = "`" | "'" | "\"" | "." | ";" | ","
    // opchar           ::= printableChar not matched by (whiteSpace | upper | lower | letter | digit | paren | delim | opchar | Unicode_Sm | Unicode_So)
    lazy val opchar = printableChar

    // printableChar    ::= // all characters in [\u0020, \u007F] inclusive
    lazy val printableChar =
      """[\u0020-\u007F]""".r
    // charEscapeSeq    ::= ‘\‘ (‘b‘ | ‘t‘ | ‘n‘ | ‘f‘ | ‘r‘ | ‘"‘ | ‘'‘ | ‘\‘)
    lazy val charEscapeSeq = "\\" ~ ("b" | "t" | "n" | "f" | "r" | "\"" | "\'" | "\\")

    // op               ::=  opchar {opchar}
    lazy val op = opchar ~ opchar.*
    // varid            ::=  lower idrest
    lazy val varid = lower ~ idrest
    // plainid          ::=  upper idrest | varid | op
    lazy val plainid = upper ~ idrest | varid | op
    // id               ::=  plainid | ‘`’ stringLiteral ‘`’
    lazy val id = plainid | "`" ~ stringLiteral ~ "`"
    // idrest           ::=  {letter | digit} [‘_’ op]
    lazy val idrest = letter | digit.* ~ ("_" ~ op).?

    // integerLiteral   ::=  (decimalNumeral | hexNumeral) [‘L’ | ‘l’]
    lazy val integerLiteral = (decimalNumeral | hexNumeral) ~ ("L" | "l").?
    // decimalNumeral   ::=  ‘0’ | nonZeroDigit {digit}
    lazy val decimalNumeral = "0" | nonZeroDigit ~ digit.*
    // hexNumeral       ::=  ‘0’ (‘x’ | ‘X’) hexDigit {hexDigit}
    lazy val hexNumeral = "0" ~ ("x" | "X") ~ UnicodeEscapes.hexDigit ~ UnicodeEscapes.hexDigit.*
    // nonZeroDigit     ::=  ‘1’ | … | ‘9’
    lazy val nonZeroDigit =
      """[1-9]""".r

    // floatingPointLiteral  ::=  digit {digit} ‘.’ digit {digit} [exponentPart] [floatType]
    //                           |  ‘.’ digit {digit} [exponentPart] [floatType]  |  digit {digit} exponentPart [floatType]
    //                           |  digit {digit} [exponentPart] floatType
    lazy val floatingPointLiteral = digit ~ digit.* ~ "." ~ digit ~ digit.* ~ exponentPart.? ~ floatType.? |
      "." ~ digit ~ digit.* ~ exponentPart.? ~ floatType.? | digit ~ digit.* ~ exponentPart ~ floatType.? |
      digit ~ digit.* ~ exponentPart.? ~ floatType
    // exponentPart     ::=  (‘E’ | ‘e’) [‘+’ | ‘-’] digit {digit}
    lazy val exponentPart = ("E" | "e") ~ ("+" | "-").? ~ digit ~ digit.*
    // floatType        ::=  ‘F’ | ‘f’ | ‘D’ | ‘d’
    lazy val floatType = "F" | "f" | "D" | "d"

    // booleanLiteral   ::=  ‘true’ | ‘false’
    lazy val booleanLiteral = "true" | "false"

    // characterLiteral ::=  ‘'’ (charNoQuoteOrNewline | UnicodeEscape | charEscapeSeq) ‘'’
    lazy val characterLiteral = "\'" ~ ("""[\u0020-\u0026]""".r | """[\u0028-\u007F]""".r | UnicodeEscapes.hexDigit | charEscapeSeq) ~ "\'"

    // stringLiteral    ::=  ‘"’ {stringElement} ‘"’  |  ‘"""’ multiLineChars ‘"""’
    lazy val stringLiteral = "\"" ~ stringElement.* ~ "\"" | "\"\"\"" ~ multiLineChars ~ "\"\"\""

    // stringElement    ::=  charNoDoubleQuoteOrNewline  |  UnicodeEscape    |  charEscapeSeq
    lazy val stringElement =  """[\u0020-\u0021]""".r | """[\u0023-\u007F]""".r | UnicodeEscapes.UnicodeEscape | charEscapeSeq
    // multiLineChars   ::=  {[‘"’] [‘"’] charNoDoubleQuote} {‘"’}
    lazy val multiLineChars = ("\"".? ~ "\"".? ~ ("""[\u0020-\u0021]""".r | """[\u0023-\u007F]""".r)).* ~ rep("\"")
    // symbolLiteral    ::=  ‘'’ plainid
    lazy val symbolLiteral = "\'" ~ plainid

    // comment          ::=  ‘/*’ “any sequence of characters; nested comments are allowed” ‘*/’
    //                        |  ‘//’ “any sequence of characters up to end of line”
    lazy val comment = "/*" ~ """(.*?)""".r ~ "*/" | "//" ~ """(.*?)""".r ~ nl

    // nl               ::=  “newlinecharacter”
    lazy val nl = "\r".? ~ "\n"
    // TODO
    // semi             ::=  ‘;’ |  nl {nl}
    lazy val semi = ";" | nl ~ nl.*


  }

}
