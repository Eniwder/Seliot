package parser

import util.ParserUtil

import scala.util.parsing.input.Positional

object ScalaParser extends ParserUtil {

  sealed trait Types
  case class IntegerType() extends Types
  case class FloatType() extends Types
  case class DoubleType() extends Types
  case class BooleanType() extends Types
  case class StringType() extends Types
  case class CharacterType() extends Types
  case class SymbolType() extends Types
  case class OtherType() extends Types


  object UnicodeEscapes {

    case class UnicodeEscapeC(str: String) extends Positional

    lazy val UnicodeEscape = positioned(
      ("\\" ~ "u" ~ rep("u") ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
        ^^ { case bs ~ u ~ ul ~ h1 ~ h2 ~ h3 ~ h4 => UnicodeEscapeC("" + bs + u + ul.mkString("") + h1 + h2 + h3 + h4) }
    )

    lazy val hexDigit = """[0-9|a-f|A-F]""".r

  }

  object LexicalSyntax {

    case class LiteralC(str: String, tpe: Types) extends Positional {


    }

    // whiteSpace       ::=  ‘\\u0020’ | ‘\\u0009’ | ‘\\u000D’ | ‘\\u000A’
    // override lazy val whiteSpace = "\\u0020" | "\\u0009" | "\\u000D" | "\\u000A"
    // upper            ::=  ‘A’ | … | ‘Z’ | ‘$’ | ‘_’  // and Unicode category Lu
    lazy val upper = """[A-Z]""".r | "$" | "_"
    // lower            ::=  ‘a’ | … | ‘z’ // and Unicode category Ll
    lazy val lower = """[a-z]""".r
    // letter           ::=  upper | lower // and Unicode categories Lo, Lt, Nl
    lazy val letter = upper | lower
    // digit            ::=  ‘0’ | … | ‘9’
    lazy val digit = """[0-9]""".r
    // paren            ::=  ‘(’ | ‘)’ | ‘[’ | ‘]’ | ‘{’ | ‘}’
    lazy val paren = "(" | ")" | "[" | "]" | "" | ".*"
    // delim            ::=  ‘`’ | ‘'’ | ‘"’ | ‘.’ | ‘;’ | ‘,’
    lazy val delim = "`" | "'" | "\"" | "." | ";" | ","
    // opchar           ::= printableChar not matched by (whiteSpace | upper | lower | letter | digit | paren | delim | opchar | Unicode_Sm | Unicode_So)
    lazy val opchar = printableChar

    // printableChar    ::= // all characters in [\u0020, \u007F] inclusive
    lazy val printableChar = """[\u0020-\u007F]""".r
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
    lazy val nonZeroDigit = """[1-9]""".r

    // floatingPointLiteral  ::=  digit {digit} ‘.’ digit {digit} [exponentPart] [floatType]
    //                           |  ‘.’ digit {digit} [exponentPart] [floatType]  |  digit {digit} exponentPart [floatType]
    //                           |  digit {digit} [exponentPart] floatType
    lazy val floatingPointLiteral = (digit ~ digit.* ~ "." ~ digit ~ digit.* ~ exponentPart.? ~ floatType.?) |
      ("." ~ digit ~ digit.* ~ exponentPart.? ~ floatType.?) | (digit ~ digit.* ~ exponentPart ~ floatType.?) |
      (digit ~ digit.* ~ exponentPart.? ~ floatType)
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
    lazy val stringElement = """[\u0020-\u0021]""".r | """[\u0023-\u007F]""".r | UnicodeEscapes.UnicodeEscape | charEscapeSeq
    // multiLineChars   ::=  {[‘"’] [‘"’] charNoDoubleQuote} {‘"’}
    lazy val multiLineChars = ("\"".? ~ "\"".? ~ ("""[\u0020-\u0021]""".r | """[\u0023-\u007F]""".r)).* ~ rep("\"")
    // symbolLiteral    ::=  ‘'’ plainid
    lazy val symbolLiteral = "\'" ~ plainid

    // comment          ::=  ‘/*’ “any sequence of characters; nested comments are allowed” ‘*/’
    //                        |  ‘//’ “any sequence of characters up to end of line”
    lazy val comment = "/*" ~ """(.*?)""".r ~ "*/" | "//" ~ """(.*?)""".r ~ nl

    // TODO nl               ::=  “newlinecharacter”
    lazy val nl = "\r".? ~ "\n"
    // semi             ::=  ‘;’ |  nl {nl}
    lazy val semi = ";" | nl ~ nl.*
    lazy val Literal = ("-".? ~ integerLiteral) | ("-".? ~ floatingPointLiteral) | booleanLiteral |
      characterLiteral | stringLiteral | symbolLiteral | "null"

  }

  object ContextFreeSyntax {

    import LexicalSyntax._

    //    Literal           ::=  [‘-’] integerLiteral  |  [‘-’] floatingPointLiteral  |  booleanLiteral
    //      |  characterLiteral  |  stringLiteral  |  symbolLiteral   |  ‘null’
    //    QualId            ::=  id {‘.’ id}
    lazy val QualId = repsep(id, ".")
    //    ids               ::=  id {‘,’ id}
    lazy val ids = repsep(id, ",")
    //    Path              ::=  StableId  |  [id ‘.’] ‘this’
    lazy val Path: Parser[_] = StableId | (id ~ ".").? ~ "this"
    //    StableId          ::=  id   |  Path ‘.’ id   |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
    lazy val StableId = id | Path ~ "." ~ id | (id ~ ".").? ~ "super" ~ ClassQualifier.? ~ "." ~ id
    //    ClassQualifier    ::=  ‘[’ id ‘]’
    lazy val ClassQualifier = "[" ~ id ~ "]"

    //    Type              ::=  FunctionArgTypes ‘=>’ Type   |  InfixType [ExistentialClause]
    lazy val Type: Parser[Any] = FunctionArgTypes ~ "=>" ~ Type | InfixType ~ ExistentialClause.?
    //      FunctionArgTypes  ::= InfixType  | ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
    lazy val FunctionArgTypes = InfixType | "(" ~ repsep(ParamType, ",").? ~ ")"
    //    ExistentialClause ::=  ‘forSome’ ‘{’ ExistentialDcl {semi ExistentialDcl} ‘}’
    lazy val ExistentialClause = "forSome" ~ "{" ~ ExistentialDcl ~ semi ~ ExistentialDcl.* ~ "}"
    //    ExistentialDcl    ::=  ‘type’ TypeDcl   |  ‘val’ ValDcl
    lazy val ExistentialDcl = "type" ~ TypeDcl | "val" ~ ValDcl
    //    InfixType         ::=  CompoundType {id [nl] CompoundType}
    lazy val InfixType = CompoundType ~ (id ~ nl.? ~ CompoundType).*
    //    CompoundType      ::=  AnnotType {‘with’ AnnotType} [Refinement]   |  Refinement
    lazy val CompoundType = AnnotType ~ "with" ~ AnnotType.* ~ Refinement.? | Refinement
    //      AnnotType         ::=  SimpleType {Annotation}
    lazy val AnnotType: Parser[Any] = SimpleType ~ AnnotType.*
    //    SimpleType        ::=  SimpleType TypeArgs   |  SimpleType ‘#’ id
    //    |  StableId   |  Path ‘.’ ‘type’   |  ‘(’ Types ‘)’
    lazy val SimpleType: Parser[Any] = SimpleType ~ TypeArgs | SimpleType ~ "#" ~ id |
      StableId | Path ~ "." ~ "type" | "(" ~ Types ~ ")"
    //    TypeArgs          ::=  ‘[’ Types ‘]’
    lazy val TypeArgs = "[" ~ Types ~ "]"
    //    Types             ::=  Type {‘,’ Type}
    lazy val Types = Type ~ repsep(",", Type)
    //    Refinement        ::=  [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
    lazy val Refinement = nl.? ~ "{" ~ RefineStat ~ semi ~ RefineStat.* ~ "}"
    //    RefineStat        ::=  Dcl   |  ‘type’ TypeDef   |
    lazy val RefineStat = Dcl | "type" ~ TypeDef | ""
    //    TypePat           ::=  Type
    lazy val TypePat = Type

    //    Ascription        ::=  ‘:’ InfixType   |  ‘:’ Annotation {Annotation}   |  ‘:’ ‘_’ ‘*’
    lazy val Ascription = ":" ~ InfixType | ":" ~ Annotation.+ | ":" ~ "_" ~ "*"

    //    Expr              ::=  (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr   |  Expr1
    lazy val Expr: Parser[Any] = (Bindings | "implicit".? ~ id | "_") ~ "=>" ~ Expr | Expr1
    //      Expr1             ::=  `if' `(' Expr `)' {nl} Expr [[semi] `else' Expr]
    //      |  `while' `(' Expr `)' {nl} Expr   |  `try' (`{' Block `}' | Expr) [`catch' `{' CaseClauses `}'] [`finally' Expr]
    //      |  `do' Expr [semi] `while' `(' Expr ')'  |  `for' (`(' Enumerators `)' | `{' Enumerators `}') {nl} [`yield'] Expr
    //      |  `throw' Expr   |  `return' [Expr]   |  [SimpleExpr `.'] id `=' Expr    |  SimpleExpr1 ArgumentExprs `=' Expr
    //      |  PostfixExpr    |  PostfixExpr Ascription   |  PostfixExpr `match' `{' CaseClauses `}'
    lazy val Expr1 = "if" ~ "(" ~ Expr ~ ")" ~ nl.* ~ Expr ~ (semi.? ~ "else" ~ Expr)
    //      PostfixExpr       ::=  InfixExpr [id [nl]]
    lazy val PostfixExpr = InfixExpr ~ (id ~ nl.?).?
    //      InfixExpr         ::=  PrefixExpr   |  InfixExpr id [nl] InfixExpr
    lazy val InfixExpr: Parser[Any] = PrefixExpr | InfixExpr ~ id ~ nl.? ~ InfixExpr
    //        PrefixExpr        ::=  [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
    lazy val PrefixExpr = ("-" | "+" | "~" | "!") ~ SimpleExpr
    //      SimpleExpr        ::=  ‘new’ (ClassTemplate | TemplateBody)
    //      |  BlockExpr   |  SimpleExpr1 [‘_’]
    lazy val SimpleExpr: Parser[Any] = "new" ~ (ClassTemplate | TemplateBody) | BlockExpr | SimpleExpr ~ "_".?
    //      SimpleExpr1       ::=  Literal  |  Path  |  ‘_’   |  ‘(’ [Exprs] ‘)’
    //      |  SimpleExpr ‘.’ id  |  SimpleExpr TypeArgs   |  SimpleExpr1 ArgumentExprs  |  XmlExpr
    lazy val SimpleExpr1: Parser[Any] = Literal | Path | "_" | "(" ~ Expr.? ~ ")" |
      SimpleExpr ~ "." ~ id | SimpleExpr ~ TypeArgs | SimpleExpr1 ~ ArgumentExprs ~ XmlExpressions.XmlExpr
    //        Exprs             ::=  Expr {‘,’ Expr}
    lazy val Exprs = repsep(Expr, ",")
    //      ArgumentExprs     ::=  ‘(’ [Exprs] ‘)’
    //      |  ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ‘)’  |  [nl] BlockExpr
    lazy val ArgumentExprs = "(" ~ Exprs.? ~ ")" |
      "(" ~ (Exprs ~ ",").? ~ PostfixExpr ~ ":" ~ "_" ~ "*" ~ ")" | nl.? ~ BlockExpr
    //        BlockExpr         ::=  ‘{’ CaseClauses ‘}’  |  ‘{’ Block ‘}’
    lazy val BlockExpr = ("{" ~ CaseClauses ~ "}") | ("{" ~ Block ~ "}")
    //      Block             ::=  BlockStat {semi BlockStat} [ResultExpr]
    lazy val Block = BlockStat ~ (semi ~ BlockStat).* ~ ResultExpr.?
    //      BlockStat         ::=  Import  |  {Annotation} [‘implicit’ | ‘lazy’] Def
    // |  {Annotation} {LocalModifier} TmplDef  |  Expr1   |
    lazy val BlockStat: Parser[Any] = Import | Annotation.? ~ ("implicit" | "lazy") ~ Def | Annotation.* ~ LocalModifier.* ~ TmplDef | ""
    //      ResultExpr        ::=  Expr1  |  (Bindings | ([‘implicit’] id | ‘_’) ‘:’ CompoundType) ‘=>’ Block
    lazy val ResultExpr: Parser[Any] = Expr1 | (Bindings | ("implicit".? ~ id | "_") ~ ":" ~ CompoundType) ~ "=>" ~ Block

    //      Enumerators       ::=  Generator {semi Generator}
    lazy val Enumerators = repsep(Generator, semi)
    //      Generator         ::=  Pattern1 ‘<-’ Expr {[semi] Guard | semi Pattern1 ‘=’ Expr}
    lazy val Generator = Pattern1 ~ "<-" ~ Expr ~ (semi.? ~ Guard | semi ~ Pattern1 ~ "=" ~ Expr)

    //      CaseClauses       ::=  CaseClause { CaseClause }
    lazy val CaseClauses = CaseClause.+
    //      CaseClause        ::=  ‘case’ Pattern [Guard] ‘=>’ Block
    lazy val CaseClause = "case" ~ Pattern ~ Guard.? ~ "=>" ~ Block
    //      Guard             ::=  ‘if’ PostfixExpr
    lazy val Guard: Parser[Any] = "if" ~ PostfixExpr

    //      Pattern           ::=  Pattern1 { ‘|’ Pattern1 }
    lazy val Pattern = repsep(Pattern1, "|")
    //      Pattern1          ::=  varid ‘:’ TypePat  |  ‘_’ ‘:’ TypePat  |  Pattern2
    lazy val Pattern1 = (varid ~ ":" ~ TypePat) | "_" ~ ":" ~ TypePat | Pattern2
    //        Pattern2          ::=  varid [‘@’ Pattern3]  |  Pattern3
    lazy val Pattern2 = varid ~ ("@" ~ Pattern3).? | Pattern3
    //        Pattern3          ::=  SimplePattern  |  SimplePattern { id [nl] SimplePattern }
    lazy val Pattern3 = SimplePattern | repsep(SimplePattern, id ~ nl.?)
    //      SimplePattern     ::=  ‘_’  |  varid  |  Literal  |  StableId
    //        |  StableId ‘(’ Patterns ‘)’  |  StableId ‘(’ [Patterns ‘,’] [varid ‘@’] ‘_’ ‘*’ ‘)’
    //      |  ‘(’ [Patterns] ‘)’  |  XmlPattern
    lazy val SimplePattern = "_" | varid | Literal | StableId |
      StableId ~ "(" ~ Patterns ~ ")" | StableId ~ "(" ~ (Patterns ~ ",").? ~ (varid ~ "@").? ~ "_" ~ "*" ~ ")" |
      "(" ~ Patterns.? ~ ")" | XmlExpressions.XmlPattern
    //        Patterns          ::=  Pattern [‘,’ Patterns]  |  ‘_’ ’*’
    lazy val Patterns: Parser[Any] = repsep(Pattern, ",") | "_" ~ "*"

    //      TypeParamClause   ::=  ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
    lazy val TypeParamClause = "[" ~ repsep(VariantTypeParam, ",") ~ "]"
    //      FunTypeParamClause::=  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
    lazy val FunTypeParamClause = "[" ~ repsep(TypeParam, ",") ~ "]"
    //      VariantTypeParam  ::=  {Annotation} [‘+’ | ‘-’] TypeParam
    lazy val VariantTypeParam: Parser[Any] = Annotation.* ~ ("+" | "-") ~ TypeParam
    //      TypeParam         ::=  (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type] {‘<%’ Type} {‘:’ Type}
    lazy val TypeParam = (id | "_") ~ TypeParamClause.? ~ (">:" ~ Type).? ~ ("<:" ~ Type).? ~ ("<%" ~ Type).* ~ (":" ~ Type).*
    //      ParamClauses      ::=  {ParamClause} [[nl] ‘(’ ‘implicit’ Params ‘)’]
    lazy val ParamClauses = ParamClause.* ~ (nl.? ~ "(" ~ "implicit" ~ Params ~ ")").?
    //      ParamClause       ::=  [nl] ‘(’ [Params] ‘)’
    lazy val ParamClause = nl.? ~ "(" ~ Params.? ~ ")"
    //      Params            ::=  Param {‘,’ Param}
    lazy val Params = repsep(Param, ",");
    //      Param             ::=  {Annotation} id [‘:’ ParamType] [‘=’ Expr]
    lazy val Param = Annotation.* ~ id ~ (":" ~ ParamType).? ~ ("=" ~ Expr).?
    //      ParamType         ::=  Type  |  ‘=>’ Type  |  Type ‘*’
    lazy val ParamType = Type | "=>" ~ Type | Type ~ "*"
    //      ClassParamClauses ::=  {ClassParamClause}  [[nl] ‘(’ ‘implicit’ ClassParams ‘)’]
    lazy val ClassParamClauses = ClassParamClause.* ~ (nl.? ~ "(" ~ "implicit" ~ ClassParams ~ ")").?
    //      ClassParamClause  ::=  [nl] ‘(’ [ClassParams] ‘)’
    lazy val ClassParamClause = nl.? ~ "(" ~ ClassParams.? ~ ")"
    //      ClassParams       ::=  ClassParam {‘,’ ClassParam}
    lazy val ClassParams = repsep(ClassParam, ",")
    //      ClassParam        ::=  {Annotation} {Modifier} [(`val' | `var')]  id ‘:’ ParamType [‘=’ Expr]
    lazy val ClassParam = Annotation.* ~ Modifier.* ~ ("val" | "var").? ~ id ~ ":" ~ ParamType ~ ("=" ~ Expr).?
    //      Bindings          ::=  ‘(’ Binding {‘,’ Binding} ‘)’
    lazy val Bindings = "(" ~ repsep(Binding, ",") ~ ")"
    //      Binding           ::=  (id | ‘_’) [‘:’ Type]
    lazy val Binding = (id | "_") ~ (":" ~ Type).?

    //      Modifier          ::=  LocalModifier  |  AccessModifier  |  ‘override’
    lazy val Modifier = LocalModifier | AccessModifier | "override"
    //      LocalModifier     ::=  ‘abstract’  |  ‘final’  |  ‘sealed’  |  ‘implicit’  |  ‘lazy’
    lazy val LocalModifier = "abstract" | "final" | "sealed" | "implicit" | "lazy"
    //      AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
    lazy val AccessModifier: Parser[Any] = ("private" | "protected") ~ AccessModifier.?
    //      AccessQualifier   ::=  ‘[’ (id | ‘this’) ‘]’
    lazy val AccessQualifier = "[" ~ (id | "this") ~ "]"

    //      Annotation        ::=  ‘@’ SimpleType {ArgumentExprs}
    lazy val Annotation = "@" ~ SimpleType ~ ArgumentExprs.*
    //      ConstrAnnotation  ::=  ‘@’ SimpleType ArgumentExprs
    lazy val ConstrAnnotation = "@" ~ SimpleType ~ ArgumentExprs

    //      TemplateBody      ::=  [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
    lazy val TemplateBody = nl.? ~ "{" ~ SelfType.? ~ repsep(TemplateStat, semi) ~ "}"
    //      TemplateStat      ::=  Import  |  {Annotation [nl]} {Modifier} Def  |  {Annotation [nl]} {Modifier} Dcl  |  Expr  |
    lazy val TemplateStat: Parser[Any] = Import | (Annotation ~ nl.?).* ~ Modifier.* ~ Def | (Annotation | nl.?).* ~ Modifier.* ~ Dcl | Expr | ""
    //      SelfType          ::=  id [‘:’ Type] ‘=>’   |  ‘this’ ‘:’ Type ‘=>’
    lazy val SelfType = id ~ (":" ~ Type).? ~ "=>" | "this" ~ ":" ~ Type ~ "=>"

    //      Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
    lazy val Import = "import" ~ repsep(ImportExpr, ",")
    //      ImportExpr        ::=  StableId ‘.’ (id | ‘_’ | ImportSelectors)
    lazy val ImportExpr = StableId ~ "." ~ (id | "_" | ImportSelector)
    //      ImportSelectors   ::=  ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’
    lazy val ImportSelectors: Parser[Any] = "{" ~ (ImportSelectors ~ ",").* ~ (ImportSelector | "_") ~ "}"
    //      ImportSelector    ::=  id [‘=>’ id | ‘=>’ ‘_’]
    lazy val ImportSelector = id ~ ("=>" ~ id | "=>" ~ "_").?

    //      Dcl               ::=  ‘val’ ValDcl  |  ‘var’ VarDcl  |  ‘def’ FunDcl  |  ‘type’ {nl} TypeDcl
    lazy val Dcl = "val" ~ ValDcl | "var" ~ VarDcl | "def" ~ FunDcl | "type" ~ nl.* ~ TypeDcl

    //      ValDcl            ::=  ids ‘:’ Type
    lazy val ValDcl = ids ~ ":" ~ Type
    //      VarDcl            ::=  ids ‘:’ Type
    lazy val VarDcl = ids ~ ":" ~ Type
    //      FunDcl            ::=  FunSig [‘:’ Type]
    lazy val FunDcl = FunSig ~ (":" ~ Type).?
    //      FunSig            ::=  id [FunTypeParamClause] ParamClauses
    lazy val FunSig = id ~ FunTypeParamClause.? ~ ParamClauses
    //        TypeDcl           ::=  id [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
    lazy val TypeDcl = id ~ TypeParamClause.? ~ (">:" ~ Type).? ~ ("<:" ~ Type)

    //      PatVarDef         ::=  ‘val’ PatDef  |  ‘var’ VarDef
    lazy val PatVarDef = "val" ~ PatDef | "var" ~ VarDef
    //      Def               ::=  PatVarDef   |  ‘def’ FunDef   |  ‘type’ {nl} TypeDef  |  TmplDef
    lazy val Def = PatVarDef | "def" ~ FunDef | "type" ~ nl.* ~ TypeDef | TmplDef
    //        PatDef            ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
    lazy val PatDef = repsep(Pattern2, ",") ~ (":" ~ Type).? ~ "=" ~ Expr
    //      VarDef            ::=  PatDef  |  ids ‘:’ Type ‘=’ ‘_’
    lazy val VarDef = PatDef | ids ~ ":" ~ Type ~ "=" ~ "_"
    //      FunDef            ::=  FunSig [‘:’ Type] ‘=’ Expr   |  FunSig [nl] ‘{’ Block ‘}’
    //      |  ‘this’ ParamClause ParamClauses   (‘=’ ConstrExpr | [nl] ConstrBlock)
    lazy val FunDef: Parser[Any] = FunSig ~ (":" ~ Type).? ~ "=" ~ Expr | FunSig ~ nl.? ~ "{" ~ Block ~ "}" |
      "this" ~ ParamClause ~ ParamClauses ~ ("=" ~ ConstrExpr | nl.? ~ ConstrBlock)
    //      TypeDef           ::=  id [TypeParamClause] ‘=’ Type
    lazy val TypeDef = id ~ TypeParamClause.? ~ "=" ~ Type

    //      TmplDef           ::=  [‘case’] ‘class’ ClassDef  |  [‘case’] ‘object’ ObjectDef  |  ‘trait’ TraitDef
    lazy val TmplDef = "case".? ~ "class".? ~ ClassDef | "case".? ~ "object" ~ ObjectDef | "trait" ~ TraitDef
    //      ClassDef          ::=  id [TypeParamClause] {ConstrAnnotation} [AccessModifier] ClassParamClauses ClassTemplateOpt
    lazy val ClassDef = id ~ TypeParamClause.? ~ ConstrAnnotation.* ~ AccessModifier.? ~ ClassParamClauses ~ ClassTemplateOpt
    //        TraitDef          ::=  id [TypeParamClause] TraitTemplateOpt
    lazy val TraitDef = id ~ TypeParamClause.? ~ TraitTemplateOpt
    //        ObjectDef         ::=  id ClassTemplateOpt
    lazy val ObjectDef = id ~ ClassTemplateOpt
    //        ClassTemplateOpt  ::=  ‘extends’ ClassTemplate | [[‘extends’] TemplateBody]
    lazy val ClassTemplateOpt = "extends" ~ ClassTemplate | "extends".? ~ TemplateBody
    //      TraitTemplateOpt  ::=  ‘extends’ TraitTemplate | [[‘extends’] TemplateBody]
    lazy val TraitTemplateOpt = "extends" ~ TraitTemplate | "extends".? ~ TemplateBody
    //      ClassTemplate     ::=  [EarlyDefs] ClassParents [TemplateBody]
    lazy val ClassTemplate = EarlyDefs.? ~ ClassParents ~ TemplateBody.?
    //      TraitTemplate     ::=  [EarlyDefs] TraitParents [TemplateBody]
    lazy val TraitTemplate = EarlyDefs.? ~ TraitParents ~ TemplateBody.?
    //      ClassParents      ::=  Constr {‘with’ AnnotType}
    lazy val ClassParents = Constr ~ ("with".? ~ AnnotType).*
    //      TraitParents      ::=  AnnotType {‘with’ AnnotType}
    lazy val TraitParents = AnnotType ~ ("with" ~ AnnotType).*
    //      Constr            ::=  AnnotType {ArgumentExprs}
    lazy val Constr: Parser[Any] = AnnotType ~ ArgumentExprs.*
    //      EarlyDefs         ::= ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ ‘with’
    lazy val EarlyDefs = "{" ~ repsep(EarlyDef, "semi") ~ "}" ~ "with"
    //      EarlyDef          ::=  {Annotation [nl]} {Modifier} PatVarDef
    lazy val EarlyDef: Parser[Any] = (Annotation ~ nl.?).* ~ Modifier.* ~ PatVarDef

    //      ConstrExpr        ::=  SelfInvocation  |  ConstrBlock
    lazy val ConstrExpr = SelfInvocation
    //        ConstrBlock       ::=  ‘{’ SelfInvocation {semi BlockStat} ‘}’
    lazy val ConstrBlock = "{" ~ SelfInvocation ~ (semi ~ BlockStat).* ~ "}"
    //      SelfInvocation    ::=  ‘this’ ArgumentExprs {ArgumentExprs}
    lazy val SelfInvocation = "this" ~ ArgumentExprs.+

    //      TopStatSeq        ::=  TopStat {semi TopStat}
    lazy val TopStatSeq: Parser[Any] = repsep(TopStat, semi)
    //      TopStat           ::=  {Annotation [nl]} {Modifier} TmplDef  |  Import  |  Packaging  |  PackageObject  |
    lazy val TopStat = (Annotation ~ nl.?).* ~ Modifier.* ~ TmplDef | Import | Packaging | PackageObject | ""
    //      Packaging         ::=  ‘package’ QualId [nl] ‘{’ TopStatSeq ‘}’
    lazy val Packaging = "package" | QualId ~ nl.? ~ "{" ~ TopStatSeq ~ "}"
    //      PackageObject     ::=  ‘package’ ‘object’ ObjectDef
    lazy val PackageObject = "package" ~ "object" ~ ObjectDef

    //      CompilationUnit   ::=  {‘package’ QualId semi} TopStatSeq
    lazy val CompilationUnit = ("package" ~ QualId ~ semi).* ~ TopStatSeq


  }

  // TODO 色々未実装
  object XmlExpressions {

    // XmlExpr ::= XmlContent {Element}
    lazy val XmlExpr = XmlContent ~ Element.*

    //    Element       ::=    EmptyElemTag   |    STag Content ETag
    lazy val Element = EmptyElemTag | STag ~ Content ~ ETag

    //    EmptyElemTag  ::=    ‘<’ Name {S Attribute} [S] ‘/>’
    lazy val EmptyElemTag = "<" ~ Name ~ (S ~ Attribute).* ~ S.? ~ "/>"

    //    STag          ::=    ‘<’ Name {S Attribute} [S] ‘>’
    lazy val STag = "<" ~ Name ~ (S ~ Attribute).* ~ S.? ~ ">"
    //    ETag          ::=    ‘</’ Name [S] ‘>’
    lazy val ETag = "</" ~ Name ~ S.? ~ ">"
    //    Content       ::=    [CharData] {Content1 [CharData]}
    lazy val Content = CharData.? ~ (Content1 ~ CharData.?).*
    //    Content1      ::=    XmlContent    |    Reference    |    ScalaExpr
    lazy val Content1 = XmlContent | Reference | ScalaExpr
    //      XmlContent    ::=    Element    |    CDSect     |    PI     |    Comment
    lazy val XmlContent: Parser[_] = ??? // Element | CDSect | PI | Comment

    //    Attribute  ::=    Name Eq AttValue
    lazy val Attribute: Parser[_] = ??? // Name ~ Eq ~ AttValue

    //    AttValue      ::=    ‘"’ {CharQ | CharRef} ‘"’   |    ‘'’ {CharA | CharRef} ‘'’   |    ScalaExpr
    lazy val AttValue: Parser[_] = ??? // "\"" ~ (CharQ | CharRef).* ~ "\"" | "\"" ~ (CharA | CharRef).* ~ "\"" | ScalaExpr

    //    ScalaExpr     ::=    Block
    lazy val ScalaExpr: Parser[_] = ??? // Block

    //    CharData      ::=   { CharNoRef }  without {CharNoRef}`{'CharB {CharNoRef}  and without {CharNoRef}`]]>'{CharNoRef}
    lazy val CharData: Parser[_] = ??? // CharaNoRef.*

    //    BaseChar, Char, Comment, CombiningChar, Ideographic, NameChar, S, Reference ::=  “as in W3C XML”
    lazy val BaseChar: Parser[_] = ???
    lazy val Char: Parser[_] = ???
    lazy val Comment: Parser[_] = ???
    lazy val CombiningChar: Parser[_] = ???
    lazy val Ideographic: Parser[_] = ???
    lazy val NameChar: Parser[_] = ???
    lazy val S: Parser[_] = ???
    lazy val Reference: Parser[_] = ???

    //    Char1         ::=  Char  without ‘<’ | ‘&’
    lazy val Char1: Parser[_] = ???
    //    CharQ         ::=  Char1  without ‘"’
    lazy val CharQ: Parser[_] = ???
    //    CharA         ::=  Char1  without ‘'’
    lazy val charA: Parser[_] = ???
    //    CharB         ::=  Char1  without ‘{’
    lazy val CharB: Parser[_] = ???

    //      Name          ::=  XNameStart {NameChar}
    lazy val Name = XNameStart ~ NameChar.*

    //      XNameStart    ::= ‘_’ | BaseChar | Ideographic  (as in W3C XML, but without  ‘:’)
    lazy val XNameStart = "_" | BaseChar | Ideographic

    // XmlPattern  ::= ElementPattern
    lazy val XmlPattern = ElementPattern

    //    ElemPattern   ::=    EmptyElemTagP     |    STagP ContentP ETagP
    lazy val ElementPattern: Parser[_] = EmptyElemTagP | STagP ~ ContentP ~ ETagP

    //    EmptyElemTagP ::=    ‘<’  Name [S] ‘/>’
    lazy val EmptyElemTagP = "<" ~ Name ~ S.? ~ "/>"
    //    STagP         ::=    ‘<’  Name [S] ‘>’
    lazy val STagP = "<" ~ Name ~ S.? ~ ">"
    //    ETagP         ::=    ‘</’ Name [S] ‘>’
    lazy val ETagP = "</" ~ Name ~ S.? ~ ">"
    //    ContentP      ::=    [CharData] {(ElemPattern|ScalaPatterns) [CharData]}
    lazy val ContentP = CharData.? ~ ((ElementPattern | ScalaPatterns) ~ CharData.?).*
    //    ContentP1     ::=    ElemPattern  |  Reference  |  CDSect   |  PI   |   Comment   |   ScalaPatterns
    lazy val ContentP1: Parser[_] = ???
    // ElemPattern | Reference | CDSect | PI | Comment | ScalaPatterns
    //      ScalaPatterns ::=    ‘{’ Patterns ‘}’
    lazy val ScalaPatterns: Parser[_] = ??? // "{" ~ Patterns ~ "}"

  }


}
