package lexer

object Keywords {

  import Tokens._

  def apply(key: String): Option[Token] = keywords get key

  val keywords = Map(
    "abstract" -> ABSTRACT,
    "case" -> CASE,
    "catch" -> CATCH,
    "class" -> CLASS,
    "def" -> DEF,
    "do" -> DO,
    "else" -> ELSE,
    "extends" -> EXTENDS,
    "false" -> FALSE,
    "final" -> FINAL,
    "finally" -> FINALLY,
    "for" -> FOR,
    "forSome" -> FORSOME,
    "if" -> IF,
    "implicit" -> IMPLICIT,
    "import" -> IMPORT,
    "lazy" -> LAZY,
    "match" -> MATCH,
    "new" -> NEW,
    "null" -> NULL,
    "object" -> OBJECT,
    "override" -> OVERRIDE,
    "package" -> PACKAGE,
    "private" -> PRIVATE,
    "protected" -> PROTECTED,
    "return" -> RETURN,
    "sealed" -> SEALED,
    "super" -> SUPER,
    "this" -> THIS,
    "throw" -> THROW,
    "trait" -> TRAIT,
    "try" -> TRY,
    "true" -> TRUE,
    "type" -> TYPE,
    "val" -> VAL,
    "var" -> VAR,
    "while" -> WHILE,
    "with" -> WITH,
    "yield" -> YIELD,
    "_" -> US,
    ":" -> CL,
    "=" -> EQ,
    "=>" -> RARROW,
    "⇒" -> RARROW,
    "<-" -> LARROW,
    "←" -> LARROW,
    "<:" -> SUBTYPE,
    "<%" -> VIEWBOUND,
    ">:" -> SUPERTYPE,
    "#" -> HASH,
    "@" -> AT,
    ";" -> SEMI
  )


}
